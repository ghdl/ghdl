library ieee;
use ieee.std_logic_1164.all;

entity dff06 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end dff06;

architecture behav of dff06 is
begin
  process (clk) is
    variable a, b : std_logic;
  begin
    if rising_edge (clk) then
      q <= b;
      b := a;
      a := d;
    end if;
  end process;
end behav;
