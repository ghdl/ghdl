library ieee;
use ieee.std_logic_1164.all;

entity unaries is
  port (
    l4 : std_logic_vector (3 downto 0);

    plus_u4u  : out std_logic_vector (3 downto 0);
    plus_s4s  : out std_logic_vector (3 downto 0);
    minus_s4s : out std_logic_vector (3 downto 0);
    abs_s4s   : out std_logic_vector (3 downto 0);

    plus_u4v  : out std_logic_vector (3 downto 0);
    plus_s4v  : out std_logic_vector (3 downto 0);
    minus_s4v : out std_logic_vector (3 downto 0);
    abs_s4v   : out std_logic_vector (3 downto 0));
end unaries;

library ieee;
use ieee.std_logic_arith.all;

architecture behav of unaries is
begin
  plus_u4u  <= std_logic_vector (unsigned'(+unsigned(l4)));
  plus_s4s  <= std_logic_vector (signed'(+signed(l4)));
  minus_s4s <= std_logic_vector (signed'(-signed(l4)));
  abs_s4s   <= std_logic_vector (signed'(abs signed(l4)));

  plus_u4v  <= +unsigned(l4);
  plus_s4v  <= +signed(l4);
  minus_s4v <= -signed(l4);
  abs_s4v   <= abs signed(l4);
end behav;
