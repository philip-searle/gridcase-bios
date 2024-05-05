#!/usr/bin/perl
use Data::Dumper;

my $USAGE= "Usage: $0 [rom_size_k] [rom_id]";
die $USAGE unless defined($ARGV[0]);
die $USAGE unless defined($ARGV[1]);

my $ROM_SIZE_K = $ARGV[0];
my $ROM_ID = $ARGV[1];

# =====================================================================

my $word_count = $ROM_SIZE_K * 1024 / 4;

for (my $word = 0; $word < $word_count; $word++) {
  print pack('N', ($ROM_ID << 24) | ($word * 4));
}

