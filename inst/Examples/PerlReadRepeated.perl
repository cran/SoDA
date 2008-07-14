sub split_repeated {
  my($input, $output, $sep) = @_;
  my($line, @value, @fields);
  while($line = <$input>) {
    @fields = split($sep, $line);
    push @value, [ @fields ];
  }
  return @value;
}
