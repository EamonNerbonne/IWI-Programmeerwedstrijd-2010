#!/usr/bin/perl

# ------------------------------------------------------------
#
# Concatenates multiple test input files
#
# Usage : perl catInput.pl test1.in test2.in test3.in > 1.in
#
# ------------------------------------------------------------

use strict;
use warnings "all";

my $count = 0;
my @lines;

# Read all files

for my $file (@ARGV) {
	open FILE, "<$file";
	my $testcount = <FILE> + 0;
	$count += $testcount;
	push @lines, <FILE>;
	close FILE;
	print STDERR " - $file:\t $testcount tests\n";
}

print STDERR " - total:\t $count tests\n";

print "$count\n";
print $_ for (@lines);
