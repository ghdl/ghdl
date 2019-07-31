library ieee;
use ieee.std_logic_1164.all;

entity dff13 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end dff13;

architecture behav of dff13 is
  signal m : std_logic;
begin
  q <= m;

  --  This is a little bit weird, but it works.
  process (clk) is
  begin
    if rising_edge (clk) then
      m <= d;
    else
      m <= m;
    end if;
  end process;
end behav;
