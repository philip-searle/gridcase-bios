#!/usr/bin/perl

use Modern::Perl;
use Config::Scoped;
use Debug::Easy;
use Term::ANSIColor qw( colored );
use Data::Dumper;
use IO::Handle;
use Struct::Dumb qw( -named_constructors );

use lib './';
use OMF::Address;
use OMF::Model;
use OMF::Parser qw( parse_omf_file );

my $log = Debug::Easy->new(
	'LogLevel' => 'INFO',
	'Color' => 1,
	'ANSILevel' => {
		'ERR'      => colored(['white on_red'],			'[ ERROR ]'),
		'WARN'     => colored(['black on_yellow'],		'[WARNING]'),
		'NOTICE'   => colored(['black on_green'],		'[NOTICE ]'),
		'INFO'     => colored(['white on_black'],		'[ INFO  ]'),
		'DEBUG'    => colored(['bold green'],			'[ DEBUG ]'),
		'DEBUGMAX' => colored(['bold black on_green'],	'[DEBUGMX]'),
	}
);

sub LOGDIE {
	my $msg = shift;
	$log->ERROR($msg);
	die "Fatal error";
}

# Links a global name to the PublicDef that defines it, as well as collecting
# all the ExternalDef records that refer to it.
struct Xref => [qw(
    name pubdef omf_externals
)], named_constructor => 1;

# Holds the final address of a global name as segment/offset and the absolute address.
struct ResolvedSymbol => [qw(
    segdef address
)], named_constructor => 1;

# Contains an LEDATA offset and a string of resolved fixup bytes to write there.
struct ResolvedFixup => [qw(
    data_record_offset data
)], named_constructor => 1;

my $cs = Config::Scoped->new( file => 'glink.lnk' );
my $config = $cs->parse;

# OmfFile instances, keyed by module_name, lowercased
my %modules = ();

# Xref instances, keyed by name
my %xrefs = ();

# ResolvedSymbol instances, keyed by unique_name (for segments)
my %resolved_symbols = ();

# List of SegmentDef records written to output (used for fixup resolution
# after all LogicalEnumeratedData has been written to the output).
my @segments_to_write = ();

# Current offset in the output's address space
my $write_ptr = OMF::Address->parse('0000:0000');

# ---------------------------------------------------------------------

sub load_input_files {
    %modules = map {
        $log->INFO("Loading OMF: $_\n");
        my $omf_file = parse_omf_file($_);
        (lc $omf_file->module_name, $omf_file);
    } @{ $config->{input}{files} };
}

# ---------------------------------------------------------------------

sub define_xref {
    my ($name, $pubdef, $omf_file) = @_;
    my $xref = $xrefs{$name};
    if (!defined $xref) {
        $xref = $xrefs{$name} = Xref(name => $name, pubdef => undef, omf_externals => []);
    }
    if (defined $pubdef) {
		$log->ERROR("Duplicate PUBDEF $name") if defined $xref->pubdef;
        $log->DEBUG("New symbol: $name\n");
        $xref->pubdef = $pubdef;
    } else {
        # Must be an external ref if no pubdef
        $log->DEBUG("New xref: $name\n");
        push @{ $xref->omf_externals }, $omf_file
    }
}

# ---------------------------------------------------------------------

sub collect_symbols {
    for my $omf_file (values %modules) {
        $log->INFO("Collecting symbols in module " . $omf_file->module_name . "\n");
        for my $extdef (@{ $omf_file->externals }) {
            next unless defined $extdef->name;
            define_xref($extdef->name, undef, $omf_file);
        }
        for my $pubdef (@{ $omf_file->public_names }) {
            next unless defined $pubdef->name;
            define_xref($pubdef->name, $pubdef, $omf_file);
        }
    }

    my @unresolved_symbols = grep { !defined($xrefs{$_}->pubdef) } keys %xrefs;
    if (@unresolved_symbols) {
        $log->ERROR("Unresolved symbols:\n");
        for my $symbol (@unresolved_symbols) {
			my $msg = "\t$symbol\treferenced from modules:";
            for my $omf_file (@{ $xrefs{$symbol}->omf_externals }) {
                $msg .= ' ' . $omf_file->module_name;
            }
            $log->WARN($msg);
        }
        LOGDIE("Cannot continue");
    }
}

# ---------------------------------------------------------------------

sub process_script_advance {
    my $params = shift;
    my $to = $params->{to};
	LOGDIE('Missing "to" in advance') unless defined $to;
    $write_ptr = OMF::Address->parse($to);
    $log->INFO("Advancing to " . $write_ptr);
}

# ---------------------------------------------------------------------

sub collect_segments {
    my $segment_name = shift;
    my $input_regex = shift;
    my @segments = ();
    my $combine_class;

    for my $omf_file (values %modules) {
        # Skip modules we don't care about
        next unless $omf_file->module_name =~ qr/^$input_regex$/in;

        my @found_segments = grep { defined $_->segment_name && $_->segment_name eq $segment_name } @{ $omf_file->segments };
        $log->ERROR("Duplicate segment for $segment_name in module " . $omf_file->module_name) if @found_segments > 1;
        if (@found_segments == 0) {
            $log->WARN("No segment matching $segment_name in module " . $omf_file->module_name);
            next;
        }

        # All segments must have the same combine class
        my $segment = $found_segments[0];
        $combine_class //= $segment->combine;
        if ($segment->combine ne $combine_class) {
            $log->ERROR( "Segment ".$segment->segment_name." in module ".$omf_file->module_name
                . " has combine type " . $segment->combine . " but expected " . $combine_class);
        }
        push @segments, $segment;
    }

    LOGDIE("No segments named $segment_name in modules matching $input_regex") unless @segments;
    return \@segments;
}

# ---------------------------------------------------------------------

my %SEGMENT_ALIGNMENTS = (
	$SEGMENT_TYPE{relocatable_byte_lseg} => 1,
	$SEGMENT_TYPE{relocatable_word_lseg} => 2,
	$SEGMENT_TYPE{relocatable_paragraph_lseg} => 16,
	$SEGMENT_TYPE{relocatable_page_lseg} => 4096
);

sub align_address {
    my ($address, $segment_type) = @_;
    my $alignment = $SEGMENT_ALIGNMENTS{$segment_type};
    LOGDIE("Don't know how to align segment type $segment_type") unless defined $alignment;
    my $aligned_address = $address->align_to($alignment);
    $log->WARN("Have to align $address to $aligned_address") unless $address == $aligned_address;
    return $aligned_address;
}

# ---------------------------------------------------------------------

sub resolve_segments {
    my $segments = shift;
    for my $segment (@{ $segments }) {
        my $aligned_write = align_address($write_ptr, $segment->type);
        $segment->alignment_padding = $aligned_write->linear_address - $write_ptr->linear_address;
        resolve_symbol($segment->unique_name, $write_ptr, $segment);
        if ($segment->combine ne 'COMMON') {
            $write_ptr = $aligned_write + $segment->length;
            $log->DEBUGMAX("Advancing write_ptr past segment (now $write_ptr), segment length = " . $segment->length);
        } else {
            $log->DEBUGMAX("COMMON segment, not advancing write_ptr\n");
        }
    }
}

# ---------------------------------------------------------------------

sub process_script_link {
    my $params = shift;
    my $input = $params->{input};
    my $segment_name = $params->{segment};
    my $virtual = $params->{virtual} // 0;
    LOGDIE('Missing "input" in link') unless defined $input;
    LOGDIE('Missing "segment" in link') unless defined $segment_name;

    my $segments = collect_segments($segment_name, $input);
    resolve_segments($segments);
    push @segments_to_write, @{ $segments } unless $virtual;
}

# ---------------------------------------------------------------------

sub process_script_fill {
    my $params = shift;
    my $to = $params->{to};
    my $with = $params->{with};
    LOGDIE('Missing "to" in fill') unless defined $to;
    LOGDIE('Missing "with" in fill') unless defined $with;

    # Synthesise a segment which contains fill data for the specified range
    $to = OMF::Address->parse($to);
    my $length = $to->linear_address - $write_ptr->linear_address;
    my $data = chr(hex($with)) x $length;
    my $ledata = LogicalEnumeratedData(offset => 0, length => $length, data => $data, fixups => [], owning_omf_file => undef, segment_index => 0);
    my $segment_name = 'FILL' . @segments_to_write;
    my $unique_name = "FILL!$segment_name";
    my $fill_segment = SegmentDef(type => $SEGMENT_TYPE{relocatable_byte_lseg},
        length => $length, combine => 'PRIVATE',
        segment_name => $segment_name,
        class_name => undef, overlay_name => undef,
        ledata => [ $ledata ], unique_name => $unique_name,
        alignment_padding => 0);
    push @segments_to_write, $fill_segment;
    resolve_symbol($unique_name, $write_ptr, $fill_segment);
    $log->INFO("Filling from $write_ptr to $to (length $length) with $with");
    $write_ptr = $to;
}

# ---------------------------------------------------------------------

sub dump_resolved_symbols {
    my $fh = shift;
    sub by_address {
        # Use a hacky fallback comparison so items at the same address are sorted
        # alphabetically unless they contain an exclamation mark, which forces them
        # to the top (so code segments appear before the first symbol they contain)
        $resolved_symbols{$a}->address <=> $resolved_symbols{$b}->address
            or ($b =~ /!/) <=> ($a =~ /!/)
            or $a cmp $b
    };
    print $fh "Resolved symbols:\n" .
        "\tResolved Address\tSymbol Name";
    for my $symbol_name (sort by_address keys %resolved_symbols) {
        print $fh "\n\t" . $resolved_symbols{$symbol_name}->address . "\t$symbol_name";
    }
    print $fh "\n";
}

# ---------------------------------------------------------------------

sub dump_segments_to_write {
    my $fh = shift;
    print $fh "Segments to write:";
    for my $segment (@segments_to_write) {
        print $fh "\n\t" . $segment->unique_name;
    }
    print $fh "\n";
}

# ---------------------------------------------------------------------

sub resolve_xrefs {
    for my $xref (values %xrefs) {
        my $symbol_name = $xref->name;
        # Predefined symbols don't need resolving
        next if (defined $resolved_symbols{$symbol_name});
        my $pubdef = $xref->pubdef;
        my $base_symbol_name = $pubdef->segment_def->unique_name;
        my $resolved_base_symbol = $resolved_symbols{$base_symbol_name};
        LOGDIE("xref $symbol_name refers to unresolved symbol $base_symbol_name") unless defined $resolved_base_symbol;
        my $resolved_xref_address = $resolved_base_symbol->address + $pubdef->offset;
        resolve_symbol($symbol_name, $resolved_xref_address, $pubdef->segment_def);
    }
}

# ---------------------------------------------------------------------

sub process_script {
    my $script = $config->{linker}{script};
	$log->INFO((@{$script} / 2) . ' script nodes to process');
    my $script_index = 0;
    while (1) {
        LOGDIE('Incomplete linker script') if $script_index > @$script - 2;
        my $action = $script->[$script_index++];
        my $params = $script->[$script_index++];
        last if $action eq 'end';
        #print "Script: $action " . Data::Dumper::Dumper($params);

        if ($action eq 'advance') {
            process_script_advance($params);
        } elsif ($action eq 'link') {
            process_script_link($params);
        } elsif ($action eq 'fill') {
            process_script_fill($params);
        } else {
            LOGDIE("Unknown script action: $action");
        }
    }
}

# ---------------------------------------------------------------------

sub resolve_fixups {
    my ($ledata, $debug_fpos, $resolved_ledata_origin) = @_;
    my @resolved_fixups = ();
    for my $fixup (@{ $ledata->fixups }) {
        # Locate target.  Secondary target methods can be reduced to primary
        # ones since the only difference is an implicit zero for the displacement.
        my $target_method = $fixup->target_method;
        my $target_displacement = $fixup->target_displacement;
        if ($target_method >= $TARGET_METHOD_TYPE{sec_segment_index}) {
            $target_method -= 4;
            $target_displacement = 0;
        }

        my $resolved_target_address;
        if ($target_method == $TARGET_METHOD_TYPE{external_index}) {
            my $external_def = $ledata->owning_omf_file->externals->[$fixup->target_datum];
            LOGDIE("Unknown external index in fixup target_datum: " . Data::Dumper::Dumper($fixup)) unless defined $external_def;
            my $resolved_symbol = $resolved_symbols{$external_def->name};
            LOGDIE("Unresolved external symbol: " . $external_def->name) unless defined $resolved_symbol;
            $resolved_target_address = $resolved_symbol->address + $target_displacement;
        } elsif ($target_method == $TARGET_METHOD_TYPE{segment_index}) {
            my $segment_def = $ledata->owning_omf_file->segments->[$fixup->target_datum];
            LOGDIE("Unknown segment index in fixup target datum: " . Data::Dumper::Dumper($fixup)) unless defined $segment_def;
            my $resolved_symbol = $resolved_symbols{$segment_def->unique_name};
            LOGDIE("Unresolved segment: " . $segment_def->unique_name) unless defined $resolved_symbol;
            $resolved_target_address = $resolved_symbol->address + $target_displacement;
        } else {
            LOGDIE("Unimplemented target_method: " . $target_method);
        }

        # Locate frame.
        my $resolved_frame_address;
        if ($fixup->frame_method == $FRAME_METHOD_TYPE{no_x_target}) {
            $resolved_frame_address = $resolved_target_address->with_zero_offset;
        } else {
            LOGDIE("Unimplemented frame_method: " . $fixup->frame_method);
        }

        # Ensure target is accessible from the given frame
        if ($resolved_target_address->linear_address < $resolved_frame_address->linear_address) {
            LOGDIE("Target $resolved_target_address is below frame origin of $resolved_frame_address");
        }
        if ($resolved_target_address->linear_address > $resolved_frame_address->linear_address + 0xFFFF) {
            LOGDIE("Target $resolved_target_address is above maximum frame address of $resolved_frame_address+0xFFFF");
        }

        # Convert to fixup data
        my $fixup_data;
        my $resolved_fixup_address = $resolved_ledata_origin + $fixup->data_record_offset;
                # if ($resolved_fixup_address >= OMF::Address->parse('F000:903F')
                #         && $resolved_fixup_address <= OMF::Address->parse('F000:9044')) {
                #     $logger->warn(Data::Dumper::Dumper($fixup));
                # }
        if ($fixup->fixup_relative_type == $FIXUP_RELATIVE_TYPE{self_relative}) {
            # Self-relative fixups are actually relative to the instruction immediately after the instruction being fixed-up.
            my $distance = $resolved_target_address->linear_address - $resolved_fixup_address->linear_address;
            if ($fixup->fixup_type == $FIXUP_TYPE{lobyte}) {
                $distance -= 1;
                if ($distance < -128 || $distance > 127) {
                    LOGDIE("Self-relative lobyte fixup outside range: $distance");
                }
                my $original_value = unpack 'c', substr($ledata->data, $fixup->data_record_offset, 1);
                $fixup_data = pack 'c', $original_value + $distance;
            } elsif ($fixup->fixup_type == $FIXUP_TYPE{offset}) {
                $distance -= 2;
                if ($distance < -32768 || $distance > 32768) {
                    $log->ERROR("Self-relative offset fixup outside range: $distance");
                    dump_fixup_data($fixup, $resolved_fixup_address, $resolved_target_address, $resolved_frame_address);
                    die;
                }
                my $original_value = unpack 'v', substr($ledata->data, $fixup->data_record_offset, 2);
                $fixup_data = pack 'v', $original_value + $distance;
            } else {
                LOGDIE("Unsupported self-relative fixup type: " . $fixup->fixup_type);
            }
        } else {

            # Segment-relative fixups are relative to the start of the segment
            my $distance = $resolved_target_address->linear_address - $resolved_frame_address->linear_address;
            if ($distance < 0 || $distance > 0xFFFF) {
                LOGDIE("Segment-relative fixup outside range: $distance");
            }
            if ($fixup->fixup_type == $FIXUP_TYPE{offset}) {
                my $original_value = unpack 'v', substr($ledata->data, $fixup->data_record_offset, 2);
                $fixup_data = pack 'v', ($original_value + $distance);
            } elsif ($fixup->fixup_type == $FIXUP_TYPE{pointer}) {
                my $original_offset = unpack 'v', substr($ledata->data, $fixup->data_record_offset, 2);
                my $original_segment = unpack 'v', substr($ledata->data, $fixup->data_record_offset + 2, 2);
                $fixup_data = pack 'vv', $original_offset + $distance, $original_segment + $resolved_frame_address->segment;
            } else {
                LOGDIE("Unimplemented segment-relative fixup type: " . $fixup->fixup_type);
            }
        }

        push @resolved_fixups, ResolvedFixup(data_record_offset => $fixup->data_record_offset, data => $fixup_data);
    }
    return @resolved_fixups;
}

sub dump_fixup_data {
    my ($fixup, $resolved_fixup_address, $resolved_target_address, $resolved_frame_address) = @_;
    print sprintf("XXX: $resolved_fixup_address\n");
    print sprintf("YYY: $resolved_target_address $resolved_frame_address\n");
    print Data::Dumper::Dumper($fixup);
}
# ---------------------------------------------------------------------

sub write_output {
    my $bin_path = $config->{output}{bin};
    my $map_path = $config->{output}{map};
	$log->INFO("Output file: $bin_path");
	$log->INFO("Map file: $map_path");
    open my $output_file, '>', $bin_path or LOGDIE("Cannot open $bin_path for writing: $!");
    open my $map_file, '>', $map_path or LOGDIE("Cannot open $map_path for writing: $!");
    binmode $output_file, ':raw';
    $output_file->autoflush;

    dump_resolved_symbols($map_file);
    dump_segments_to_write($map_file);
    close $map_file;

    for my $segment (@segments_to_write) {
        $log->DEBUG("Writing segment " . $segment->unique_name);
        my $offset = 0;
        for my $ledata (@{ $segment->ledata }) {
            $log->DEBUGMAX("Writing " . $ledata->length . " bytes at offset " . $ledata->offset);
            my $data_record_origin = tell $output_file;
            my $resolved_ledata_origin = $resolved_symbols{$segment->unique_name}->address + $ledata->offset;
            print $output_file $ledata->data;
            for my $resolved_fixup (resolve_fixups($ledata, $data_record_origin, $resolved_ledata_origin)) {
                seek $output_file, $data_record_origin + $resolved_fixup->data_record_offset, 0;
                print $output_file $resolved_fixup->data;
            }
            seek $output_file, $data_record_origin + $ledata->length, 0;
            $offset += $ledata->length;
        }
        print $output_file 0x90 x $segment->alignment_padding;
        LOGDIE("Gap after ledata records at offset $offset") unless $offset == $segment->length;
    }
}

# ---------------------------------------------------------------------

sub resolve_symbol {
    my ($symbol_name, $address, $segdef) = @_;
    my $resolved_symbol = ResolvedSymbol(segdef => $segdef, address => $address);
    LOGDIE("Duplicate symbol resolved: $symbol_name") if defined $resolved_symbols{$symbol_name};
    return $resolved_symbols{$symbol_name} = $resolved_symbol;
}

# ---------------------------------------------------------------------

sub predefine_symbols {
    my $definitions = $config->{linker}{define};
    while (my ($symbol_name, $address) = each %{ $definitions }) {
        $address = OMF::Address->parse($address);
		$log->INFO("Predefined $address as $symbol_name");
        my $pubdef = PublicDef(name => $symbol_name,
            group_name => undef,
            segment_def => undef,
            offset => $address->offset, type => undef);
        define_xref($symbol_name, $pubdef);

        # Resolve symbol immediately
        resolve_symbol($symbol_name, $address);
    }
}

# ---------------------------------------------------------------------

sub verify_asserts {
	my $assertions_failed = 0;
	my $asserts = $config->{linker}{assert};

	while (my ($symbol, $expected_address) = each %{ $asserts }) {
		my $resolved_symbol = $resolved_symbols{$symbol} || $resolved_symbols{"~$symbol"};
		if (!defined $resolved_symbol) {
			$log->WARN("Symbol not found for assertion: $symbol = $expected_address");
			$assertions_failed++;
			next;
		}
		if (OMF::Address->parse($expected_address) != $resolved_symbol->address) {
			$log->WARN("Symbol $symbol expected at $expected_address but was at " . $resolved_symbol->address);
			$assertions_failed++;
			next;
		}
	}

	LOGDIE("Compatibility assertions failed: $assertions_failed") unless $assertions_failed == 0;
	$log->INFO("Assertions passed: " . %{ $asserts });
}

# ---------------------------------------------------------------------

sub main {
	$log->NOTICE('Predefining symbols');
    predefine_symbols;
	$log->NOTICE('Loading input files');
    load_input_files;
	$log->NOTICE('Collecting symbols');
    collect_symbols;
	$log->NOTICE('Processing linker script');
    process_script;
	$log->NOTICE('Resolving cross-references');
    resolve_xrefs;
	$log->NOTICE('Writing output file');
    write_output;

	$log->NOTICE('Verifying assertions');
	verify_asserts;
}

# ---------------------------------------------------------------------

main;
