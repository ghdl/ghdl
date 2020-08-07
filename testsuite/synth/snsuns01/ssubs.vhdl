library ieee;
use ieee.std_logic_1164.all;

entity subs is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    sub_v4v3 : out std_logic_vector (3 downto 0);
    sub_v4i  : out std_logic_vector (3 downto 0);
    sub_iv3  : out std_logic_vector (2 downto 0);
    sub_v4l  : out std_logic_vector (3 downto 0);
    sub_lv3  : out std_logic_vector (2 downto 0));
end subs;

library ieee;
use ieee.std_logic_signed.all;

architecture behav of subs is
begin
  sub_v4v3 <= l4 - r3;
  sub_v4i <= l4 - ri;
  sub_iv3 <= li - r3;
  sub_v4l <= l4 - r3(0);
  sub_lv3 <= l4(0) - r3;
end behav;
