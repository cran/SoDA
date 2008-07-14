our $last_table;

sub hash_and_count_words {
  my($tref, @lines) = @_;
  my(@words, %table, $line, $word);

  %table = %{$tref};
  foreach $line (@lines) {
    @words = split("[^a-zA-Z]+", $line);
    foreach $word (@words) {
      $table{$word} = $table{$word} + 1;
    }
  }
  $last_table = \%table;
  return \%table;
}

sub more_hash {
 
my(@words, %table, $line, $word);

  %table = %{$last_table};
  foreach $line (@_) {
    @words = split("[^a-zA-Z]+", $line);
    foreach $word (@words) {
      $table{$word} = $table{$word} + 1;
    }
  }
  $last_table = \%table;
  return \%table;
}
  
1;
