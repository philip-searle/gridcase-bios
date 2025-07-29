#!/usr/bin/perl

use strict;
use warnings;
use autodie;

die "Usage: $0 [rom-file]" unless $ARGV[0];

open my $fh, '<:raw', $ARGV[0];

my $checksum_even = 0;
my $checksum_odd = 0;
while (1) {
    my $success = read $fh, (my $bytes), 2;
    last if not $success;

	my ($byte_even, $byte_odd) = unpack "CC", $bytes;
	$checksum_even += $byte_even;
	$checksum_odd += $byte_odd;
}
print sprintf(<<OUTPUT, $checksum_even, $checksum_odd);
 ROM file: $ARGV[0]
Checksums:
     Even: %02x
      Odd: %02x
OUTPUT
close $fh;
