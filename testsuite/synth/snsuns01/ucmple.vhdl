library ieee;
use ieee.std_logic_1164.all;

entity cmple is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    le_v4v3 : out boolean;
    le_v4i  : out boolean;
    le_iv3  : out boolean);
end cmple;

library ieee;
use ieee.std_logic_unsigned.all;

architecture behav of cmple is
begin
  le_v4v3 <= l4 <= r3;
  le_v4i  <= l4 <= ri;
  le_iv3  <= li <= r3;
end behav;
