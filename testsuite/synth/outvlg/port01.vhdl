library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity port01 is
  port (clk : std_logic;
        dir : std_logic;
        val : std_logic_vector (2 downto 0);
        io : inout std_logic);
end port01;

architecture behav of port01 is
  signal r1 : std_logic;
begin
  io <= 'Z' when dir = '1'else r1;
  
  process (clk)
  begin
    if falling_edge(clk) then
      r1 <= val(0) xnor val(1);
    end if;
  end process;
end behav;
