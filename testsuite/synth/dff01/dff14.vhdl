library ieee;
use ieee.std_logic_1164.all;

entity dff14 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end dff14;

architecture behav of dff14 is
begin
  process (clk) is
    variable m : std_logic;
  begin
    if rising_edge (clk) then
      m := d;
    end if;
    q <= m;
  end process;
end behav;
