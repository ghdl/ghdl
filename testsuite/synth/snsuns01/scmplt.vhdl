library ieee;
use ieee.std_logic_1164.all;

entity cmplt is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    lt_v4v3 : out boolean;
    lt_v4i  : out boolean;
    lt_iv3  : out boolean);
end cmplt;

library ieee;
use ieee.std_logic_signed.all;

architecture behav of cmplt is
begin
  lt_v4v3 <= l4 < r3;
  lt_v4i  <= l4 < ri;
  lt_iv3  <= li < r3;
end behav;
