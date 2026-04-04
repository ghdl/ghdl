library ieee;
use ieee.std_logic_1164.all;

entity foo is port (
  a : in  std_logic;
  y : out std_logic);
end entity;

architecture beh of foo is

component bar is
end component;
attribute test : string;
attribute test of bar: component is "yes";

begin
   y <= a;

   b0 : bar;
end beh;
