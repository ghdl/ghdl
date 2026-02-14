library std;
use std.textio.all;

library ieee;
use ieee.math_real.all;

entity test_64bit is

end entity test_64bit;

architecture sim of test_64bit is

  procedure print(
    str : string
  ) is
    variable ln : line;
  begin
    write(ln, str);
    writeline(output, ln);
  end procedure print;

begin

  run_P: process is
    variable ihi : integer;
    variable ilo : integer;
    variable ibw : integer;
  begin
    ihi := integer'high;
    ilo := integer'low;
    ibw := integer(ceil(log2(real(ihi) - real(ilo))));

    print(
      string'("Int: ")    &
      to_string(ilo)      &
      string'(" .. ")     &
      to_string(ihi)      &
      LF                  &
      string'("  bits: ") &
      to_string(ibw)
    );

    wait;
  end process run_P;

end architecture sim;
