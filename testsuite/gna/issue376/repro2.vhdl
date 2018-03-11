library ieee;
use ieee.std_logic_1164.all;

entity repro2 is
  port (p : inout std_logic_vector (3 downto 0));
end repro2;

architecture behav of repro2 is
  alias p_0 is p(0);
begin
  p_0 <= '0' after 1 ns, '1' after 4 ns;
end behav;
