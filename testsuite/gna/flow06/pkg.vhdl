package myconfig is
  constant WIDTH : natural := 16;
  constant DEPTH : natural := 256;
  constant NAME  : string  := "core";
end package;

library ieee;
use ieee.std_logic_1164.all;
use work.myconfig.all;

entity pkgtop is
end pkgtop;

architecture rtl of pkgtop is
  constant LOCAL : natural := WIDTH + 1;
  signal s : std_logic_vector (WIDTH-1 downto 0);
begin
end rtl;
