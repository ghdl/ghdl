library ieee;
use ieee.std_logic_1164.all;

entity unaries is
  port (
    l4 : std_logic_vector (3 downto 0);

    plus_v    : out std_logic_vector (3 downto 0);
    minus_v   : out std_logic_vector (3 downto 0);
    abs_v     : out std_logic_vector (3 downto 0));
end unaries;

library ieee;
use ieee.std_logic_signed.all;

architecture behav of unaries is
begin
  plus_v  <= +l4;
  minus_v <= -l4;
  abs_v   <= abs l4;
end behav;
