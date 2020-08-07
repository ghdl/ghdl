library ieee;
use ieee.std_logic_1164.all;

entity muls is
  port (
    l3 : std_logic_vector (2 downto 0);
    r4 : std_logic_vector (3 downto 0);

    mul_v3v4 : out std_logic_vector (6 downto 0));
end muls;

library ieee;
use ieee.std_logic_signed.all;

architecture behav of muls is
begin
  mul_v3v4 <= l3 * r4;
end behav;
