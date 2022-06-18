
package OMF::Model;

use Modern::Perl;
use Carp;
use Struct::Dumb qw( -named_constructors );

use base 'Exporter';
our @EXPORT = qw(
    %SEGMENT_TYPE %SEGMENT_COMBINE_TYPE
    %FIXUP_TYPE %FRAME_METHOD_TYPE %TARGET_METHOD_TYPE %FIXUP_RELATIVE_TYPE
    lookup_record_type
    RecordHeader
    SegmentDef ExternalDef PublicDef LogicalEnumeratedData FixupDef
    OmfExtDependency OmfExtensions
    OmfFile
    create_omf_file
);

# ---------------------------------------------------------------------

struct RecordType => [qw(
    sixcc type name
)], named_constructor => 0;

my %OBJ_RECORD_TYPE = ();

sub register_record_type {
    my $record_type = RecordType(@_);
    $OBJ_RECORD_TYPE{$record_type->type} = $record_type;
}

register_record_type('RHEADR', 0x6E, 'R-Module Header');
register_record_type('REGINT', 0x70, 'Register Initialization');
register_record_type('REDATA', 0x72, 'Relocatable Enumerated Data');
register_record_type('RIDATA', 0x74, 'Relocatable Iterated Data');
register_record_type('OVLDEF', 0x76, 'Overlay Definition');
register_record_type('ENDREC', 0x78, 'End of recordset');
register_record_type('BLKDEF', 0x7A, 'Block Definition');
register_record_type('BLKEND', 0x7C, 'Block End');
register_record_type('DEBSYM', 0x7E, 'Debug Symbols');
register_record_type('THEADR', 0x80, 'Translator Header');
register_record_type('LHEADR', 0x82, 'Library Module Header');
register_record_type('PEDATA', 0x84, 'Physical Enumerated Data');
register_record_type('PIDATA', 0x86, 'Physical Iterated Data');
register_record_type('COMENT', 0x88, 'Comment');
register_record_type('MODEND', 0x8A, 'Module End');
register_record_type('EXTDEF', 0x8C, 'External Names Definition');
register_record_type('TYPDEF', 0x8E, 'Type Definition');
register_record_type('PUBDEF', 0x90, 'Public Names Definition');
register_record_type('LOCSYM', 0x92, 'Local Symbols');
register_record_type('LINNUM', 0x94, 'Line Numbers');
register_record_type('LNAMES', 0x96, 'List of Names');
register_record_type('SEGDEF', 0x98, 'Segment Definition');
register_record_type('GRPDEF', 0x9A, 'Group Definition');
register_record_type('FIXUPP', 0x9C, 'Fixup');
register_record_type('UNUSED', 0x9E, 'Unused record type');
register_record_type('LEDATA', 0xA0, 'Logical Enumerated Data');
register_record_type('LIDATA', 0xA2, 'Logical Iterated Data');
register_record_type('LIBHED', 0xA4, 'Library Header');
register_record_type('LIBNAM', 0xA6, 'Library Module Names');
register_record_type('LIBLOC', 0xA8, 'Library Dictionary');
register_record_type('COMDEF', 0xB0, 'Communal Names Definition');
register_record_type('BAKPAT', 0xB2, 'Backpatch');

# ---------------------------------------------------------------------

sub lookup_record_type {
    my $type = shift;
    my $record_type = $OBJ_RECORD_TYPE{$type} or die "Unknown OMF record type $type";
    return $record_type;
}

# ---------------------------------------------------------------------

our %SEGMENT_TYPE = (
    absolute_lseg               => 0,
	relocatable_byte_lseg       => 1,
	relocatable_word_lseg       => 2,
	relocatable_paragraph_lseg  => 3,
	relocatable_page_lseg       => 4,
	absolute_mas                => 5,
	ltl_paragraph_lseg          => 6
);

our %SEGMENT_COMBINE_TYPE = (
    0 => 'PRIVATE',     # Do not combine with any other program segment
    1 => 'RESERVED',
    2 => 'PUBLIC',      # Combine by appending at an offset which meets the alignment requirement
    3 => 'RESERVED',
    4 => 'PUBLIC',
    5 => 'STACK',       # Combine as for PUBLIC
    6 => 'COMMON',      # Combine by overlay using maximum size
    7 => 'PUBLIC'
);

our %FIXUP_TYPE = (
	lobyte  => 0,
	offset  => 1,
	base    => 2,
	pointer => 3,
	hibyte  => 4
);

our %FIXUP_RELATIVE_TYPE = (
    self_relative => 0,
    segment_relative => 1
);

our %FRAME_METHOD_TYPE = (
    # X is a segment index: frame is a canonic frame of the LSEG defined by the index.
	segment_index => 0,

	# X is a group index: frame is the canonic frame defined by the group
	group_index => 1,

	# X is an external index: frame is determined when external name's public
	# definition is found (see 8086 Object Module Format, pg. 16 for details
	# of F2a, F2b, F2c cases).
	external_index => 2,

	# X is a frame number.
	frame_number => 3,

	# No X: frame is the canonic frame of the LSEG containing LOCATION.
	no_x_lseg => 4,

	# No X: frame is determined by the target.  See 8086 Object Module Foramt,
	# pg. 16 for details of cases F5a, F5b, F5c, F5d.
	no_x_target => 5,

	# No X: no frame (8089 only).
	no_x_no_frame => 6
);

our %TARGET_METHOD_TYPE = (
	# Primary: target is a segment index, i.e. D'th byte in the LSEG
	# identifed by the index.
	segment_index => 0,

	# Primary: target is a group index, i.e. D'th byte following the first
	# byte in the LSEG in the group that is eventually LOCATE'd lowest in
	# memory address space.
	group_index => 1,

	# Primary: target is an external index, i.e. D'th byte following the byte
	# whose address is (eventually) given by the external name identified by
	# the index.
	external_index => 2,

	# Primary: target is a frame number, i.e. D'th  byte in the frame
	# identified by the frame number (that is the address of the target is
	# X*16+D).
	frame_number => 3,

	# Secondary: as segment_index but displacement is implicitly zero.
	sec_segment_index => 4,

	# Secondary: as group_index but displacement is implicitly zero.
	sec_group_index => 5,

	# Secondary: as external_index but displacement is implicitly zero.
	sec_external_index => 6,

	# Secondary: as frame_number but displacement is implicitly zero.
	sec_frame_number => 7
);

# ---------------------------------------------------------------------

struct RecordHeader => [qw(
    type length
)], named_constructor => 1;

# alignment_padding is used at runtime to store length of space needed
# after this segment in order to align the following segment correctly
struct SegmentDef => [qw(
    type length combine
    segment_name class_name overlay_name
    ledata unique_name
    alignment_padding
)], named_constructor => 1;

struct ExternalDef => [qw(
    name type
)], named_constructor => 1;

struct PublicDef => [qw(
    name group_name segment_def offset type
)], named_constructor => 1;

# owning_omf_file is used at runtime to link back to the omf_file which
# contains this ledata record so we can access the ExternalDefs
struct LogicalEnumeratedData => [qw(
    offset length data fixups
    owning_omf_file
)], named_constructor => 1;

struct FixupDef => [qw(
    fixup_type fixup_relative_type data_record_offset
    frame_method target_method frame_datum target_datum
    target_displacement
)], named_constructor => 1;

struct OmfExtDependency => [qw(
    timestamp file_path
)], named_constructor => 1;

struct OmfExtensions => [qw(
    compiler_info
    cpu_model_info
    dependencies
    import_definitions
    export_definitions
)], named_constructor => 1;

struct OmfFile => [qw(
    module_name
    names
    segments
    public_names
    externals
    le_data
    extensions

    last_ledata
)], named_constructor => 1;

# ---------------------------------------------------------------------

sub create_omf_file {
    my $extensions = OmfExtensions(compiler_info => undef,
        cpu_model_info => undef, dependencies => [],
        import_definitions => [], export_definitions => []);
    my $omf_file = OmfFile(module_name => undef,
        names => [], segments => [], public_names => [], externals => [],
        le_data => [],  extensions => $extensions,
        last_ledata => undef);

    # Zero is not a valid index for record types name, segment, and external.
    # Insert placeholder entries to occupy those slots.
    push @{ $omf_file->names }, undef;
    my $segment0 = SegmentDef(
        segment_name => $omf_file->names->[0],
        type => undef,
        combine => undef,
        class_name => undef,
        overlay_name => undef,
        length => undef,
        ledata => [], unique_name => undef, alignment_padding => 0
    );
    push @{ $omf_file->segments }, $segment0;
    my $external0 = ExternalDef(name => $omf_file->names->[0], type => '<undef>');
    push @{ $omf_file->externals }, $external0;

    return $omf_file;
}

# ---------------------------------------------------------------------

1;
