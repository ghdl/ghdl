library ieee;
use ieee.std_logic_1164.all;

entity bug3 is
end;

architecture arch of bug3 is
begin
  process
    variable res: std_ulogic;
    variable n : natural;
  begin
    for a in std_ulogic range '0' to '1' loop
      for b in std_ulogic range '0' to '1' loop
        with a&b select
          res := '0' when "0U"|"U0"|"11",
          '1' when "U1"|"1U"|"00",
          'U' when others;
        wait for 1 ns;
        report "a=" & std_ulogic'image(a) & ", b=" & std_ulogic'image(b);
        n := n + 1;
      end loop;
    end loop;
    assert n = 4 report "bad number of iterations" severity failure;
    wait;
  end process;
end;
