sub fields_to_lines {
  my($input, $output, $nfield,
     $sep, $outSep, $lineSepAs) = @_;
  my($line, @fields, $fieldNo, $lineNo);
  ## read input until there's enough for a line of output
  while (@fields_to_lines_buf  < $nfield) {
    if(!($line = <$input>)) {
       return(0); # end of input
    }
    chomp($line);
     @fields = split($sep, "$line$lineSepAs");
    if ($sep != $lineSepAs) {
      ## append first field of this line to last field in buffer
      my($last) = pop(@fields_to_lines_buf).shift(@fields);
      push(@fields_to_lines_buf, $last);
    }
    @fields_to_lines_buf = (@fields_to_lines_buf, @fields);
  }
  ## now write out as many lines as there are
   while (@fields_to_lines_buf  >= $nfield) {
     for ($fieldNo = 1; $fieldNo < $nfield; $fieldNo++) {
      print $output shift(@fields_to_lines_buf),$outSep;
    }
    print $output shift(@fields_to_lines_buf),"\n";
  }
  return(1); # not the end of the input
}

@fields_to_lines_buf = ();
1;
