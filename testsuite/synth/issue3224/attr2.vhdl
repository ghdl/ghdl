library ieee;
use ieee.std_logic_1164.all;

entity attr2_sub is
  port (
  a : in  std_logic;
  y : out std_logic);
end entity;

architecture behav of attr2_sub is
begin
  y <= not a;
end;
  
library ieee;
use ieee.std_logic_1164.all;

entity attr2 is port (
  a : in  std_logic;
  y : out std_logic);
end entity;

architecture beh of attr2 is

  component attr2_sub is
    port (
      a : in  std_logic;
      y : out std_logic);
  end component;
  attribute test : string;
  attribute test of attr2_sub: component is "yes";

begin
   b0 : attr2_sub port map (a, y);
end beh;
