library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
  port(
    input : in unsigned := "0011");
end entity;

architecture rtl of test is
  signal copy : input'subtype;
begin
end architecture;

