library ieee;
use ieee.std_logic_1164.all;

entity shrs is
  port (
    l3 : std_logic_vector (2 downto 0);
    r4 : std_logic_vector (3 downto 0);

    shl_v3v4 : out std_logic_vector (2 downto 0);
    shr_v3v4 : out std_logic_vector (2 downto 0));
end shrs;

library ieee;
use ieee.std_logic_unsigned.all;

architecture behav of shrs is
begin
  shl_v3v4 <= shl(l3, r4);
  shr_v3v4 <= shr(l3, r4);
end behav;
