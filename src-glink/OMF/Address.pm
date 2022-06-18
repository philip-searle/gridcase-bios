package OMF::Address;

use Modern::Perl;
use Carp;

use base 'Exporter';
our @EXPORT = qw(
    parse with_zero_offset
);

# ---------------------------------------------------------------------

sub new {
    my ($class, $segment, $offset) = @_;
    return bless [$segment, $offset] => $class;
}

# ---------------------------------------------------------------------

sub parse {
    my ($class, $input) = @_;

    if ($input =~ /^([0-9A-F]{1,}):([0-9A-F]{1,4})$/i) {
        return new($class, hex($1), hex($2));
    }
    die "Invalid X86 address: $input";
}

# ---------------------------------------------------------------------

sub with_zero_offset {
    my $self = shift;
    return OMF::Address->new($self->[0], 0);
}

# ---------------------------------------------------------------------

sub linear_address {
    my $self = shift;
    return ($self->[0] << 4) + $self->[1];
}

# ---------------------------------------------------------------------

sub segment {
    return shift->[0];
}

# ---------------------------------------------------------------------

sub offset {
    return shift->[1];
}

# ---------------------------------------------------------------------

# $alignment must be a power of two
sub align_to {
    my ($self, $alignment) = @_;
    my $mask = $alignment - 1;
    my $increment = (($self->[1] + $mask) & ~$mask) - $self->[1];
    return $self + $increment;
}

# ---------------------------------------------------------------------

sub to_string {
    my $self = shift;
    return sprintf('%04X:%04X', $self->[0], $self->[1]);
}

# ---------------------------------------------------------------------

sub plus {
    my $self = shift;
    my $addend = shift or 0;
    my $segment = $self->[0];
    my $offset = $self->[1] + $addend;
    if ($offset > 0x10000) {
        die "Incrementing $self by $addend would wrap past segment end";
    }
    return OMF::Address->new($segment, $offset);
}

# ---------------------------------------------------------------------

sub threeway_comparison {
    my $self = shift;
    my $other = shift;
    return $self->[0] <=> $other->[0] || $self->[1] <=> $other->[1];
}

# ---------------------------------------------------------------------

use overload fallback => 1,
    '""' => 'to_string',
    '+' => 'plus',
    '<=>' => 'threeway_comparison';

# ---------------------------------------------------------------------

1;
