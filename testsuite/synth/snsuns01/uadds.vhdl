library ieee;
use ieee.std_logic_1164.all;

entity adds is
  port (
    li : integer;
    ri : integer;
    l4 : std_logic_vector (3 downto 0);
    r3 : std_logic_vector (2 downto 0);

    add_v4v3 : out std_logic_vector (3 downto 0);
    add_v4i  : out std_logic_vector (3 downto 0);
    add_iv3  : out std_logic_vector (2 downto 0);
    add_v4l  : out std_logic_vector (3 downto 0);
    add_lv3  : out std_logic_vector (2 downto 0));
end adds;

library ieee;
use ieee.std_logic_unsigned.all;

architecture behav of adds is
begin
  add_v4v3 <= l4 + r3;
  add_v4i <= l4 + ri;
  add_iv3 <= li + r3;
  add_v4l <= l4 + r3(0);
  add_lv3 <= l4(0) + r3;
end behav;
