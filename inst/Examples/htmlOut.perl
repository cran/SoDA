$infile = shift;
open(INPUT, "<$infile") or 
   die "Can't open input \"$infile\"";
$outfile = shift;
open(OUTPUT, ">$outfile") or
  die "Can't open output \"$outfile\"";

while($line = <INPUT>) {
    $line =~ s/<[^>]*>//g;
    print OUTPUT $line;
}
