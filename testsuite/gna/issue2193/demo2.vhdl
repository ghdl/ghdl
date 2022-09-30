library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;

entity demo2 is
end demo2;

architecture behavioral of demo2 is
    signal clk  : std_logic := '0';
    signal i    : integer   := 0;
begin

  clk <= not clk after 5 ns;

  process(clk)
    variable l : line;
    variable s : string(1 to 4) := "    ";
  begin
    if rising_edge(clk) then
      if i = 1 then
        std.env.finish;
      end if;
      l := new string(1 to 13);
      l.all := "hereisastring";
      report l.all; -- fine
      report l.all(1 to 4); -- fine
      read(l, s); -- fine if commented out
      report s;
      report l.all; -- fine
      report l.all(5 to 13); -- overflow detected
      i <= i+1;
    end if;
  end process;

end behavioral;
