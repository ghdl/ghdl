library ieee;
use ieee.std_logic_1164.all;

entity shrs is
  port (
    l3 : std_logic_vector (2 downto 0);
    r4 : std_logic_vector (3 downto 0);

    shl_u3u4u : out std_logic_vector (2 downto 0);
    shl_s3u4s : out std_logic_vector (2 downto 0);
    shr_u3u4u : out std_logic_vector (2 downto 0);
    shr_s3u4s : out std_logic_vector (2 downto 0));
end shrs;

library ieee;
use ieee.std_logic_arith.all;

architecture behav of shrs is
begin
  shl_u3u4u <= std_logic_vector (shl(unsigned(l3), unsigned(r4)));
  shl_s3u4s <= std_logic_vector (shl(signed(l3), unsigned(r4)));
  shr_u3u4u <= std_logic_vector (shr(unsigned(l3), unsigned(r4)));
  shr_s3u4s <= std_logic_vector (shr(signed(l3), unsigned(r4)));
end behav;
