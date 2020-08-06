library ieee;
use ieee.std_logic_1164.all;

entity cmpge is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    ge_u4u3 : out boolean;
    ge_s4s3 : out boolean;
    ge_u4s3 : out boolean;
    ge_s4u3 : out boolean;
    ge_u4i  : out boolean;
    ge_iu3  : out boolean;
    ge_s4i  : out boolean;
    ge_is3  : out boolean);
end cmpge;

library ieee;
use ieee.std_logic_arith.all;

architecture behav of cmpge is
begin
  ge_u4u3 <= unsigned(l4) >= unsigned(r3);
  ge_s4s3 <= signed(l4) >= signed(r3);
  ge_u4s3 <= unsigned(l4) >= signed(r3);
  ge_s4u3 <= signed(l4) >= unsigned(r3);
  ge_u4i  <= unsigned(l4) >= ri;
  ge_iu3  <= li >= unsigned(r3);
  ge_s4i  <= signed(l4) >= ri;
  ge_is3  <= li >= signed(r3);
end behav;
