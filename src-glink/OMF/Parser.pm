
package OMF::Parser;

use Modern::Perl;
use Carp;
use OMF::Model;

use base 'Exporter';
our @EXPORT = qw( parse_omf_file );

# If set, parser will ignore the 'length' field of LEDATA records and instead
# assume that consecutive LEDATA records should be layed out sequentially in
# the output file.  This works around a bug in EuroASM 20190402 where the
# length field actually contains the size of the previous LEDATA record.
my $EUROASM_LEDATA_WORKAROUND = 1;

# ---------------------------------------------------------------------

sub lookup_name {
    my ($omf_file, $name_index) = @_;
    return $name_index ? $omf_file->names->[$name_index] : undef;
}

# ---------------------------------------------------------------------

sub unpack_fh {
    my ($fh, $length, $format) = @_;
    read $fh, (my $buf), $length;
    return unpack $format, $buf;
}

# ---------------------------------------------------------------------

sub read_string {
    my ($fh, $length) = @_;
    return unpack_fh $fh, $length, "a[$length]";
}

# ---------------------------------------------------------------------

sub read_name {
    my $fh = shift;
    my $length = unpack_fh $fh, 1, 'C';
    return read_string($fh, $length);
}

# ---------------------------------------------------------------------

sub name_byte_count {
    my $name = shift;
    return length($name) + 1;
}

# ---------------------------------------------------------------------

sub read_index {
    my $fh = shift;
    my $value_low = unpack_fh $fh, 1, 'C';
    my $value_high = 0;
    if ($value_low & 0x80) {
        $value_high = $value_low & 0x7F;
        $value_low = unpack_fh $fh, 1, 'C';
    }
    return ($value_high << 8) + $value_low;
}

# ---------------------------------------------------------------------

sub index_byte_count {
    my $index = shift;
    return $index > 0x7F ? 2 : 1;
}

# ---------------------------------------------------------------------

sub read_record_header {
    my $fh = shift;
    my ($type, $length) = unpack_fh $fh, 1 + 2, 'CS';
    my $parsed_type = lookup_record_type($type);

    # Subtract one because we don't want to read the record trailer
    return RecordHeader(type => $parsed_type, length => $length - 1);
}

# ---------------------------------------------------------------------

sub skip_record_trailer {
    my $fh = shift;
    # Discard checksum byte, return length
    read $fh, my $buf, 1;
    return 1;
}

# ---------------------------------------------------------------------

sub parse_record_theadr {
    my ($fh, $length, $omf_file) = @_;
    die "THEADR record found after module name already set" if defined $omf_file->module_name;
    $omf_file->module_name = read_name($fh);
    $length -= name_byte_count($omf_file->module_name);
    skip_record_trailer($fh);
    die "Inconsistent THEADR record length: $length" if $length != 0;
}

# ---------------------------------------------------------------------

sub parse_record_coment {
    my ($fh, $length, $omf_file) = @_;
    my ($flags, $comment_class) = unpack_fh $fh, 1 + 1, 'CC';
    $length -= 1 + 1;
    my $comment = read_string($fh, $length);
    $length -= length($comment);
    skip_record_trailer($fh);

    if ($comment_class == 0x00) {
        # Compiler info, text string
        die 'Duplicate compiler info found' if defined $omf_file->extensions->compiler_info;
        $omf_file->extensions->compiler_info = $comment;
    } elsif ($comment_class == 0x9D) {
        # Two char comment:
        #   CPU:   '0' - '6', '7'     == 8086 - '686, x64
        #   Model: 's', 'c', 'm', 'l' == small, medium, compact, large
        die 'Duplicate cpu/memory model info found' if defined $omf_file->extensions->cpu_model_info;
        die "Unexpected CPU info comment length: " . length($comment) unless length($comment) == 2;
        my ($cpu, $model) = split '', $comment;
        die "Unknown CPU type: $cpu" unless $cpu =~ /[0-6]/;
        die "Unknown memory model: $model" unless $model =~ /[smcl]/;
        $omf_file->extensions->cpu_model_info = $comment;
    } elsif ($comment_class == 0xE9) {
        # Skip dependency record for main file
        return if length($comment) == 0;
        die "Too short file dependency comment: " . length($comment) if length($comment) < 5;
        my ($timestamp, $file_path) = unpack 'VC/a', $comment;
        push @{ $omf_file->extensions->dependencies }, OmfExtDependency(
            timestamp => $timestamp, file_path => $file_path);
    } else {
        $comment =~ s/[^\w\s]/./g;
        die "Unknown COMENT: $flags $comment_class $comment\n";
    }
}

# ---------------------------------------------------------------------

sub parse_record_lnames {
    my ($fh, $length, $omf_file) = @_;
    my @lnames = ();
    while ($length > 0) {
        my $lname = read_name($fh);
        $length -= name_byte_count($lname);
        push @lnames, $lname;
    }
    skip_record_trailer($fh);
    push @{ $omf_file->names }, @lnames;
    die "Bad LNAMES length: $length" unless $length == 0;
}

# ---------------------------------------------------------------------

sub parse_record_segdef {
    my ($fh, $length, $omf_file) = @_;

    # Read and parse ACBP bitfield
    my $acbp = unpack_fh $fh, 1, 'C';
    my $segment_type = ($acbp >> 5) & 7;
    my $segment_combine = ($acbp >> 2) & 7;
    my $is_big = ($acbp >> 1) & 1;
    my $page_resident = $acbp & 1;

    # Handle optional fields
    if ($segment_type == $SEGMENT_TYPE{absolute_lseg} ||
        $segment_type == $SEGMENT_TYPE{absolute_mas}) {
            # Would have frame number and offset fields
            die "unsupported type $segment_type";
    } elsif ($segment_type == $SEGMENT_TYPE{ltl_paragraph_lseg}) {
        # Would have LTL Dat, maximum segment length, and group offset fields
        die "unsupported type $segment_type";
    }

    die "page_resident segment not supported" if $page_resident;

    my $segment_length = unpack_fh $fh, 2, 'v';
    if ($is_big) {
        die "non-zero segment length not supported for big segments" unless $segment_length == 0;
        $segment_length = 0x10000;
    }

    my ($segment_name, $class_name, $overlay_name, $unique_name);
    if ($segment_type != $SEGMENT_TYPE{absolute_mas}) {
        $segment_name = lookup_name($omf_file, read_index($fh));
        $class_name = lookup_name($omf_file, read_index($fh));
        $overlay_name = lookup_name($omf_file, read_index($fh));
        # unique_name is used to disambiguate segments from different OmfFile instances
        # e.g. where we need to store all resolved segment addresses for a linked program
        $unique_name = $omf_file->module_name . '!' . $segment_name;
    }

    my $segment = SegmentDef(type => $segment_type,
        length => $segment_length, combine => $SEGMENT_COMBINE_TYPE{$segment_combine},
        segment_name => $segment_name,
        class_name => $class_name,
        overlay_name => $overlay_name,
        ledata => [], unique_name => $unique_name, alignment_padding => 0);
    skip_record_trailer($fh);
    push @{ $omf_file->segments }, $segment;
}

# ---------------------------------------------------------------------

sub skip_record_grpdef {
	my ($fh, $length, $omf_file) = @_;

	my $grpdef_name = read_name($fh);
	$length -= name_byte_count($grpdef_name);
    read $fh, (my $buf), $length;
	skip_record_trailer($fh);
}

# ---------------------------------------------------------------------

sub parse_record_extdef {
    my ($fh, $length, $omf_file) = @_;

    while ($length > 0) {
        my $extdef_name = read_name($fh);
        my $extdef_type = read_index($fh);
        $length -= name_byte_count($extdef_name);
        $length -= index_byte_count($extdef_type);
        push @{ $omf_file->externals }, ExternalDef(
            name => $extdef_name, type => lookup_name($omf_file, $extdef_type));
    }
    skip_record_trailer($fh);
    die "Inconsistent EXTDEF length: $length" unless $length == 0;
}

# ---------------------------------------------------------------------

sub parse_record_pubdef {
    my ($fh, $length, $omf_file) = @_;

    my $group_index = read_index($fh);
    my $segment_index = read_index($fh);
    $length -= index_byte_count($group_index) + index_byte_count($segment_index);
    die "frame_number not supported for pubdef record" if $group_index == 0 && $segment_index == 0;

    while ($length > 0) {
        my $name = read_name($fh);
        my $offset = unpack_fh $fh, 2, 'v';
        my $type_index = read_index($fh);
        $length -= name_byte_count($name) + 2 + index_byte_count($type_index);
        push @{ $omf_file->public_names }, PublicDef(name => $name,
            group_name => lookup_name($group_index),
            segment_def => $omf_file->segments->[$segment_index],
            offset => $offset, type => lookup_name($type_index));
    }
    skip_record_trailer($fh);
}

# ---------------------------------------------------------------------

sub parse_record_ledata {
    my ($fh, $length, $omf_file) = @_;
    my $segment_index = read_index($fh);
    my $offset = unpack_fh $fh, 2, 'v';
    $length -= index_byte_count($segment_index) + 2;

    if ($EUROASM_LEDATA_WORKAROUND && defined($omf_file->last_ledata) && $omf_file->last_ledata->segment_index == $segment_index) {
        # Assume offset is immediately after previous LEDATA record ends
        $offset = $omf_file->last_ledata->offset + $omf_file->last_ledata->length;
    }

    my $data = unpack_fh $fh, $length, "a[$length]";
    my $ledata = LogicalEnumeratedData(offset => $offset,
        length => $length, data => $data, fixups => [],
        owning_omf_file => $omf_file, segment_index => $segment_index);
    push @{ $omf_file->segments->[$segment_index]->ledata }, $ledata;
    $omf_file->last_ledata = $ledata;
    skip_record_trailer($fh);
}

# ---------------------------------------------------------------------

sub parse_record_fixupp {
    my ($fh, $length, $omf_file) = @_;
    while ($length > 0) {
        #print "FIXUP@ ".sprintf('%08x', tell $fh) . "\n";
        my $initial_byte = unpack_fh $fh, 1, 'C';
        $length -= 1;
        die 'FIXUPP record with TRD DAT = 0 not supported' unless ($initial_byte & 0x80);

        my @locat = ($initial_byte, unpack_fh($fh, 1, 'C'));
        $length -= 1;
        die 'LOCAT with S set not supported' if ($locat[0] & 0x20);
        my $fixup_type = ($locat[0] >> 2) & 0x07;
        my $fixup_relative_to = $locat[0] & 0x40 ?
            $FIXUP_RELATIVE_TYPE{segment_relative} :
            $FIXUP_RELATIVE_TYPE{self_relative};
        my $fixup_data_record_offset = (($locat[0] & 0x03) << 8) | $locat[1];

        my $fixdat = unpack_fh $fh, 1, 'C';
        $length -= 1;
        die 'FIXDAT with F set not supported' if ($fixdat & 0x80);
        die 'FIXDAT with T set not supported' if ($fixdat & 0x08);
        my $frame_method = ($fixdat >> 4) & 0x07;
        my $target_method = ($fixdat >> 0) & 0x07;

        # Frame datum is present only when the frame is specified neither by a
        # thread (F=0) nor explicitly by methods no_x_lseg, no_x_target, or
        # no_x_no_frame.
        my $frame_datum = undef;
        unless ($frame_method == $FRAME_METHOD_TYPE{no_x_lseg} ||
                $frame_method == $FRAME_METHOD_TYPE{no_x_target} ||
                $frame_method == $FRAME_METHOD_TYPE{no_x_no_frame}) {
                    $frame_datum = read_index($fh);
                    $length -= index_byte_count($frame_datum);
        }

        # Target datum is present only when the target is not specified by a thread.
        my $target_datum = read_index($fh);
        $length -= index_byte_count($target_datum);

        # Target displacement is only present for primary methods of specifying targets.
        my $target_displacement = 0;
        if ($target_method == $TARGET_METHOD_TYPE{segment_index} ||
            $target_method == $TARGET_METHOD_TYPE{group_index} ||
            $target_method == $TARGET_METHOD_TYPE{external_index} ||
            $target_method == $TARGET_METHOD_TYPE{frame_number}) {
                $target_displacement = unpack_fh $fh, 2, 'v';
                $length -= 2;
        }

        push @{ $omf_file->last_ledata->fixups }, FixupDef(
            fixup_type => $fixup_type, fixup_relative_type => $fixup_relative_to,
            data_record_offset => $fixup_data_record_offset,
            frame_method => $frame_method, target_method => $target_method,
            frame_datum => $frame_datum, target_datum => $target_datum,
            target_displacement => $target_displacement
        );
    }

    skip_record_trailer($fh);
}

# ---------------------------------------------------------------------

sub parse_record_modend {
    my ($fh, $length, $omf_file) = @_;
    my $mod_type = unpack_fh $fh, 1 ,'C';
    die 'MODEND with mod_type != 0 not supported' unless $mod_type == 0;
    skip_record_trailer($fh);
}

# ---------------------------------------------------------------------

my %PARSE_HANDLERS = (
    THEADR => \&parse_record_theadr,
    COMENT => \&parse_record_coment,
    LNAMES => \&parse_record_lnames,
    SEGDEF => \&parse_record_segdef,
	GRPDEF => \&skip_record_grpdef,
    EXTDEF => \&parse_record_extdef,
    PUBDEF => \&parse_record_pubdef,
    LEDATA => \&parse_record_ledata,
    FIXUPP => \&parse_record_fixupp,
    MODEND => \&parse_record_modend
);

sub parse_omf_file {
    my ($filename) = @_;
    my $omf_file = create_omf_file();

    open my $fh, '<', $filename or die "Couldn't open $filename: $!";
    binmode $fh, ':raw';

    while (!eof($fh)) {
        my $header = read_record_header($fh);
        # print "Record: " . Data::Dumper::Dumper($header);
        my $handler = $PARSE_HANDLERS{$header->type->sixcc};
        if (!defined $handler) {
            #print "XXL:".$omf_file->last_ledata->offset."\n\n";
            die "Unimplemented record type: " . $header->type->sixcc;
        }
        $handler->($fh, $header->length, $omf_file);
    }

    return $omf_file;
}

# ---------------------------------------------------------------------

1;
