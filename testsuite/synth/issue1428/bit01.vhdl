library ieee;
use ieee.std_logic_1164.all;

entity bit01 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of bit01 is
  signal s : std_logic;
begin
  s <= b (1);
  s <= b(2);
  a <= not s;
end behav;
