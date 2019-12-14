library ieee;
use ieee.std_logic_1164.all;

entity test3 is
  port (val : out std_logic_vector (7 downto 0));
end entity test3;

architecture beh of test3 is
begin

  val <= "Z001101X";
end architecture beh;
