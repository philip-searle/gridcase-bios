#!/usr/bin/perl

use Modern::Perl;
use Data::Dumper;

use lib './';
use OMF::Model;
use OMF::Parser qw( parse_omf_file );

# ---------------------------------------------------------------------

sub dump_name {
    my $name = shift;
    return defined($name) ? $name : "<undef>";
}

# ---------------------------------------------------------------------

sub dump_dos_datetime {
    my $value = shift;
    my $date = ($value & 0xFFFF0000) >> 16;
    my $time = ($value & 0x0000FFFF) >> 0;

    my $day     = ($date >>  0) & 0x1F;
    my $month   = ($date >>  5) & 0x0F;
    my $year    = ($date >>  9) & 0x7F;

    my $seconds = ($time >>  0) & 0x1F;
    my $minutes = ($time >>  5) & 0x3F;
    my $hours   = ($time >> 10) & 0x1F;

    return sprintf('%04d-%02d-%02d %02d:%02d:%02d',
        $year + 1980, $month, $day, $hours, $minutes, $seconds * 2);
}

# ---------------------------------------------------------------------

sub dump_ledata {
    my $ledata_list = shift;
    my $real_bytes = 0;
    my $fixups = 0;
    for my $ledata (@{ $ledata_list }) {
        $real_bytes += $ledata->length;
        $fixups += @{ $ledata->fixups };
        #print Data::Dumper::Dumper($ledata->fixups);
        #print "XXX: " . $ledata->offset . " " . $ledata->length . "\n";
    }
    return ($real_bytes ? sprintf('%04X bytes ledata', $real_bytes) : '')
        . ($fixups ? " $fixups fixups" : '');
}

# ---------------------------------------------------------------------

sub dump_omf_file {
    my $omf_file = shift;
    print "OMF Dump";
    print "\nModule name\t\t" . dump_name($omf_file->module_name);
    print "\nSegments\t\tType\tCombine\tClass\tLength\tOverlay\tName";
    for my $segdef (@{ $omf_file->segments }) {
        next if !defined($segdef->segment_name);
        print "\n\t\t\t" . dump_name($segdef->type)
            . "\t" . $segdef->combine
            . "\t" . dump_name($segdef->class_name)
            . "\t" . sprintf('  %4X', $segdef->length)
            . "\t" . dump_name($segdef->overlay_name)
            . "\t" . dump_name($segdef->segment_name)
            . "\t" . dump_ledata($segdef->ledata);
    }
    print "\nPublic names\t\tGroup\tseg:offset\tType\tName";
    for my $pubdef (@{ $omf_file->public_names }) {
        print "\n\t\t\t" . dump_name($pubdef->group_name)
            . "\t" . $pubdef->segment_def->segment_name . ":" . sprintf('%04X', $pubdef->offset)
            . "\t" . dump_name($pubdef->type) . "\t" . $pubdef->name;
    }
    print "\nExternals\t\tType\tName";
    for my $extdef (@{ $omf_file->externals }) {
        next if !defined($extdef->name);
        print "\n\t\t\t" . dump_name($extdef->type) . "\t" . $extdef->name;
    }
    print "\nExtensions:";
    print "\n\tCompiler info\t" . $omf_file->extensions->compiler_info;
    print "\n\tCPU/mem model\t" . $omf_file->extensions->cpu_model_info;
    print "\n\tDependencies\tTimestamp           Path";
    for my $file_dependency (@{ $omf_file->extensions->dependencies }) {
        print "\n\t\t\t" . dump_dos_datetime($file_dependency->timestamp) . " " . $file_dependency->file_path;
    }
    print "\n";
}

# ---------------------------------------------------------------------

for (@ARGV) {
    dump_omf_file parse_omf_file $_;
}
