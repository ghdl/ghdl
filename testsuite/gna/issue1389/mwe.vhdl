library ieee;
use ieee.math_real.all;

entity mwe is
end entity;

architecture a of mwe is
begin
  process
    constant hr_real  : real := 60.0**2.0;
    constant div_real : real := 1.0e-15;
    constant expect   : real := 3.6e18;
    variable got      : real;
    variable err      : real;
  begin
    got := hr_real/div_real;
    err := abs(got-expect);
    report "hr_real=" & real'image(hr_real);
    assert got = expect
      report
        "got=" & real'image(got) &LF&
        "expect=" & real'image(expect) &LF&
        "err=" & real'image(err)
      severity error;
    wait;
  end process;
end;
