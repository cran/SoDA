%dateActions = ( 735 => 1, 525 => 1, 334 => 1, # continue
      124 => 2, 214 => 2, 421 => 2); # done
# anything else is an error

@savedLines = ();

sub findDateForm {
    my $nextFun = $_[0];
    my @dmy = (1+2+4, 1+2, 1+4),
       $action, @line;
    while(@line = &{$nextFun}()) {
	push @savedLines [ @line ];
	@dmy = testDateForm(\@line, \@dmy);
	## check for termination or errors
	if(@dmy != 3) {return ;}
	else {
	    $action = $dateActions{join("",@dmy)};
	    if($action != 1) {last;}
	}
    }
    $action == 2 ? @dmy : undef;
}

1;
