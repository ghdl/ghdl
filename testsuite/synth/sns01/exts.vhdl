library ieee;
use ieee.std_logic_1164.all;

entity exts is
  port (
    l3 : std_logic_vector (2 downto 0);

    ext_u3 : out std_logic_vector (4 downto 0);
    sxt_s3 : out std_logic_vector (4 downto 0));
end exts;

library ieee;
use ieee.std_logic_arith.all;

architecture behav of exts is
begin
  ext_u3 <= ext(l3, 5);
  sxt_s3 <= sxt(l3, 5);
end behav;
