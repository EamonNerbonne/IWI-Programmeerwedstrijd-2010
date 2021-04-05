
use strict;

if ($#ARGV < 0) {
	die "Usage: perl $0 <ID>\n";
}

my $id = $ARGV[0]; # Adventures of Huckleberry Finn by Mark Twain
my $url = "http://www.gutenberg.org/files/$id/$id.txt";
my $tempfile = "download-$id.tmp";
my $outfile = "gutenberg-$id.txt";

# Download it
if (!-e $tempfile) {
	print "Downloading $url\n";
	print `wget $url -O $tempfile`;
}

# Read it
my $text;
{
	local $/ = undef;
	open FILE, "$tempfile";
	$text = <FILE>;
	close FILE;
}

# Filter it
print "Filtering book $id, length: ",length($text),"\n";
$text =~ s/\r//g; # die windows, die

# remove junk
$text =~ s/.*\*\*\* START OF THIS PROJECT[^\n]*\n//s;
$text =~ s/\nEnd of (the )?Project Gutenberg.*//s;
$text =~ s/\n\*\*\* END OF THIS PROJECT.*//s;
$text =~ s/^CHAPTER [IXVC0-9]+//g;
$text =~ s/^TABLE OF CONTENTS$//g;

#$text = lc $text;
$text =~ s/'//g; # insignificant punctuation
$text =~ s/[^0-9A-Za-z. \n]/ /g; # punctuation better replaced by whitespace

$text =~ s/\n/ /g;
#$text =~ s/(\S)[ ]*\n[ ]*(\S)/$1 $2/gs; # join paragraphs into to single lines paragraphs
#$text =~ s/\s*\n\s*/\n/gs; # join paragraphs
$text =~ s/ +/ /g;
$text =~ s/^ +//g;
$text =~ s/ +$//g;

$text = substr $text,0,999999;
$text .= "\n";

# Save it
print "Saving book $id to $outfile, length: ",length($text),"\n";
open FILE, ">$outfile";
print FILE $text;
close FILE;
