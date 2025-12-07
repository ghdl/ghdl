library ieee;
use ieee.std_logic_1164.all;

entity asgn10 is
  port (a, b : std_logic_vector (1 downto 0);
        sel_a : std_logic;
        sel_b : std_logic;
        o : out std_logic_vector (1 downto 0));
end asgn10;

architecture behav of asgn10 is
begin
  o <= a when sel_a = '1' else
       b when sel_b = '1' else
       unaffected;
end behav;
