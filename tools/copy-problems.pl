
use strict;

my $contest = $ARGV[0];
my $prefix = $contest eq 'test' ? 'T' : '';

mkdir "inout";
mkdir "inout/$contest";
mkdir "inout/$contest/testdata";
mkdir "inout/$contest/example";
mkdir "inout/$contest/example-licht";
mkdir "inout/$contest/solutions";

sub find_nice_problem_name {
	my($problem_name) = @_;
	my $nice_name = '';
	open PROBLEMFILE, "<problems/$problem_name/text/problem.tex";
	while (<PROBLEMFILE>) {
		if (/\\Problem\{(.*)\}/) {
			$nice_name = $1;
		}
	}
	close PROBLEMFILE;
	return $nice_name;
}

my $i = 0;

open TEXFILE, "<contests/$contest.tex";
while (<TEXFILE>) {
	if (/^\\inputproblem\{(.*)\}/) {
		my $problem_name = $1;
		my $name = $prefix . chr($i + ord('A'));
		my $nice_problem_name = find_nice_problem_name($problem_name);
		print "Copying input/output: $name. $nice_problem_name ($problem_name)\n";
		mkdir "inout/$contest/testdata/$name";
		mkdir "inout/$contest/example/$name";
		mkdir "inout/$contest/example-licht/$name";
		mkdir "inout/$contest/solutions/$name";
		#$nice_problem_name =~ s/[ ]/\\ /g;
		#$nice_problem_name =~ s/[']/\\'/g;
		`touch "inout/$contest/example/$name/($nice_problem_name)"`;
		`touch "inout/$contest/example-licht/$name/($nice_problem_name)"`;
		`touch "inout/$contest/testdata/$name/($nice_problem_name)"`;
		`touch "inout/$contest/solutions/$name/($nice_problem_name)"`;
		`cp -u problems/$problem_name/input/example.{in,out}  inout/$contest/example/$name`;
		`cp -u problems/$problem_name/input/testdata.{in,out} inout/$contest/testdata/$name`;
		`cp -u problems/$problem_name/impl/*.{java,c,cpp,cs,hs,rb,ape} inout/$contest/solutions/$name 2> /dev/null`;
		if (-f "problems/$problem_name/input/testdata.in-licht") {
		`cp -u problems/$problem_name/input/testdata.{in,out}-licht inout/$contest/testdata/$name`;
		}
		if (-f "problems/$problem_name/input/example.in-licht") {
		`cp -u problems/$problem_name/input/example.in-licht inout/$contest/example-licht/$name/example.in`;
		`cp -u problems/$problem_name/input/example.out-licht inout/$contest/example-licht/$name/example.out`;
		} else {
		`cp -u problems/$problem_name/input/example.{in,out}  inout/$contest/example-licht/$name`;
		}
		print "  input md5:  ", `md5sum inout/$contest/testdata/$name/testdata.in  | cut -c-32`;
		print "  output md5: ", `md5sum inout/$contest/testdata/$name/testdata.out | cut -c-32`;
		$i++;
	}
}
close TEXFILE;

print "\n";
print "=====================================================================\n";
print "Test data is now stored in:   inout/$contest/testdata\n";
print "Exampe data is now stored in: inout/$contest/example\n";
print "Reference solutions are in:   inout/$contest/solutions\n";
print "=====================================================================\n";
print "\n";
