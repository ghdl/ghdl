library ieee;
use ieee.std_logic_1164.all;

entity bug2 is
end;

architecture arch of bug2 is
  signal a, b, r : std_ulogic;

  function foo (a, b: std_ulogic) return std_ulogic is
    variable res: std_ulogic;
  begin
    with a&b select
      res := '0' when "0U"|"U0"|"11",
      '1' when "U1"|"1U"|"00",
      'U' when others;
    return res;
  end function;
begin

  r <= foo (a, b);

  process
  begin
    (a, b) <= std_logic_vector'("00");
    wait for 1 ns;
    report "a=" & std_ulogic'image(a) & ", b=" & std_ulogic'image(b);
    (a, b) <= std_logic_vector'("11");
    wait for 1 ns;
    report "a=" & std_ulogic'image(a) & ", b=" & std_ulogic'image(b);
    wait;
  end process;
end;
