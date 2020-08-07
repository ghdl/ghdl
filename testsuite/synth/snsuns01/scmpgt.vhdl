library ieee;
use ieee.std_logic_1164.all;

entity cmpgt is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    gt_v4v3 : out boolean;
    gt_v4i  : out boolean;
    gt_iv3  : out boolean);
end cmpgt;

library ieee;
use ieee.std_logic_signed.all;

architecture behav of cmpgt is
begin
  gt_v4v3 <= l4 > r3;
  gt_v4i  <= l4 > ri;
  gt_iv3  <= li > r3;
end behav;
