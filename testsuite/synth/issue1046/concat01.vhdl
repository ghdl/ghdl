library ieee;
use ieee.std_logic_1164.all;

entity concat01 is
  generic (a : std_logic_vector (7 downto 0) := x"ab";
           b : std_logic_vector (7 downto 0) := x"9e");
  port (res : out std_logic_vector (15 downto 0));
end concat01;

architecture behav of concat01 is
  constant c : std_logic_vector := a & b;
begin
  res <= c;
end behav;
