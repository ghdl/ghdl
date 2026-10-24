entity repro is
end;

architecture behav of repro is
  procedure check (fmt : string; val : real; res : string)
  is
    constant str : string := to_string(val, fmt);
  begin
    assert str = res
      report "bad result for fmt '" & fmt & "', get '" & str
         & "' but expect '" & res & ''' severity failure;
  end check;
begin
  process
  begin
    --  %f/F: write in decimal notation [-]ddd.ddd, with trailing 0.
    --  The precision specifies the number of decimal places.
    --  Default precision is 6.
    check ("%f", 1.5, "1.500000");
    check ("%F", 1.5, "1.500000");
    check ("%F", -1.5, "-1.500000");
    check ("%+f", 1.5, "+1.500000");
    check ("% f", 1.5, " 1.500000");
    check ("%+ f", 1.5, "+1.500000");
    check ("% f", -1.5, "-1.500000");
    check ("%.0f", 1.5, "2");
    check ("%#.0f", 1.5, "2.");
    check ("%05.2f", 1.5, "01.50");
    check ("%.2f", 1.5, "1.50");
    check ("%5.2f", 1.5, " 1.50");
    check ("%-5.2f", 1.5, "1.50 ");

    check ("%E", 1.5, "1.500000E+00");
    check ("%e", 1.5, "1.500000e+00");
    check ("%.3e", 1.5, "1.500e+00");
    
    --  Bad format
    check ("%d", 1.0, "*BAD-FORMAT*");
    check ("%*f", 1.0, "*BAD-FORMAT*");
    check ("f", 1.0, "*BAD-FORMAT*");
    check ("%lf", 1.0, "*BAD-FORMAT*");
    wait;
  end process;
end;
