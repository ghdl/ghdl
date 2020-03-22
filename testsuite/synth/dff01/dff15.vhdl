library ieee;
use ieee.std_logic_1164.all;

entity dff15 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end dff15;

architecture behav of dff15 is
begin
  process (clk) is
    variable m : std_logic;
  begin
    if rising_edge (clk) then
      m := d;
    end if;
    q <= not m;
  end process;
end behav;
