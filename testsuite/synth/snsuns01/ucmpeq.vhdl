library ieee;
use ieee.std_logic_1164.all;

entity cmpeq is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    eq_v4v3 : out boolean;
    eq_v4i  : out boolean;
    eq_iv3  : out boolean);
end cmpeq;

library ieee;
use ieee.std_logic_unsigned.all;

architecture behav of cmpeq is
begin
  eq_v4v3 <= l4 = r3;
  eq_v4i  <= l4 = ri;
  eq_iv3  <= li = r3;
end behav;
