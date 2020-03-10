library ieee;
use ieee.std_logic_1164.all;

entity slice01 is
  port (di : std_logic_vector(7 downto 0);
        do : out std_logic_vector (3 downto 0));
end slice01;

architecture behav of slice01 is
  type mem is array (natural range <>) of std_logic_vector (1 downto 0);
  signal m1, m2 : mem (3 downto 0);
begin
  m1 <= (di (7 downto 6), di (5 downto 4), di (3 downto 2), di (1 downto 0));
  m2 <= (m1 (0), m1 (1), m1 (2), m1 (3));
  do <= m2 (1) & m2 (0);
end behav;
