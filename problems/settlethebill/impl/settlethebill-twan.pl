<>;
$|=1;
while (<>) {
	($n,$m) = split;
	my @p=();
	$p[$_] =0 for (0..$n-1);
	for (1..$m) {
		$_=<>;
		($a,$b,$x)=split;
		$p[$a]+=$x;
		$p[$b]-=$x;
	}
	#print "$n $m\n";
	#print "$_ " for (@p); print "\n";
	# all possible sums
	my %sums=(0=>1);
	for $i (0..$n-1) {
		my %sums2=%sums;
		while (($a,$b)=each(%sums2)) {
			$sums{$a+$p[$i]} += $b;
			#print "$a: $b\n";
		}
	}
	#print "$_:",$sums{$_}," " for (keys %sums); print "\n";
	print ($sums{0} == 2 ? "tight\n" : "loose\n");
	#print "bla:",$sums{0}, "\n";
}
