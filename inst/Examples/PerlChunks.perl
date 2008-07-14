sub chunks_add {
    my($tref, @chunks) = @_;
    my($chunk);

    foreach $chunk (@chunks) {
      $$tref{$chunk} = $$tref{$chunk} + 1;
    }
    return $tref;
  }

sub chunks_drop {
    my($tref, @chunks) = @_;
    my( $chunk, $count);

    foreach $chunk (@chunks) {
      $count = $$tref{$chunk} - 1;

      if($count > 0) { $$tref{$chunk} = $count;}
      elsif ($count == 0)  {delete $$tref{$chunk}; }
      else {die 
	"Decrementing a chunk (\"$chunk\") not in the table";}
    }
    return $tref;
  }
