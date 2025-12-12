library ieee;
use ieee.std_logic_1164.all;

entity match03 is
  port (a : in std_logic_vector (39 downto 0);
        z : out std_logic);
end match03;

architecture behav of match03 is
begin
  z <= a ?= b"-10-_1111_1110_1101_1100_1011_1010_1001_1000_0--1";
end behav;
