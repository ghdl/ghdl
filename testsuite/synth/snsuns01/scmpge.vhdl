library ieee;
use ieee.std_logic_1164.all;

entity cmpge is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    ge_v4v3 : out boolean;
    ge_v4i  : out boolean;
    ge_iv3  : out boolean);
end cmpge;

library ieee;
use ieee.std_logic_signed.all;

architecture behav of cmpge is
begin
  ge_v4v3 <= l4 >= r3;
  ge_v4i  <= l4 >= ri;
  ge_iv3  <= li >= r3;
end behav;
