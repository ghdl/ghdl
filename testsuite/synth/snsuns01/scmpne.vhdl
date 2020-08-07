library ieee;
use ieee.std_logic_1164.all;

entity cmpne is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    ne_v4v3 : out boolean;
    ne_v4i  : out boolean;
    ne_iv3  : out boolean);
end cmpne;

library ieee;
use ieee.std_logic_signed.all;

architecture behav of cmpne is
begin
  ne_v4v3 <= l4 /= r3;
  ne_v4i  <= l4 /= ri;
  ne_iv3  <= li /= r3;
end behav;
