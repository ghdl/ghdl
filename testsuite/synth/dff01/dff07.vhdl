library ieee;
use ieee.std_logic_1164.all;

entity dff07 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end dff07;

architecture behav of dff07 is
begin
  process (clk) is
    variable a, b : std_logic;
  begin
    if rising_edge (clk) then
      a := d;
      b := a;
      q <= b;
    end if;
  end process;
end behav;
