%roman = (
	  i => "01", ii=>"02", iii=>"03", iv=>"04", v=>"05",
	  vi=>"06", vii=>"07", viii=>"08", ix=>"09", x=>10,
	  xi=>11, xii=>12);

sub monthRoman {
    $roman{$_[0]};
}

sub monthText {
    $Months{$_[0]};
}

sub monthNumber {
    my $string = $_[0];
    $string =~ /^([1-9]|1[12]|0[1-9])$/ ?
	$string+0 : undef;
}

sub dayNumber {
    my $string = $_[0];
    $string =~ /^(0[1-9]|[12]\d|3[01])$/ ?
	$string : 
	($string =~ /^[1-9]$/ ? "0$string" : undef);
}

sub year4Digit {
    my $string = $_[0];
    $string =~ /^\d{4}$/ ?
	$string : undef;
}

## will choose 20th or 21st century, whichever
## is nearer:  this year + 50 is the boundary
$bdyYear = ((localtime)[5]) % 100 + 49;

sub year2Digit {
    my $string = $_[0];
    if($string !~ /^\d{2}$/) {
	return ;
    }
    my $year = $string + 0;
    ($year < $bdyYear) ? (2000 + $year) : (1900 + $year);
}

@dayFuns = (\&dayNumber);
@monthFuns = (\&monthText, \&monthNumber, \&monthRoman);
@yearFuns = (\&year4Digit, \&year2Digit);
