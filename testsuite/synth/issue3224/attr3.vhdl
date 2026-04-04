library ieee;
use ieee.std_logic_1164.all;

entity attr3_sub is
  port (
  a : in  std_logic;
  y : out std_logic);
end entity;

architecture behav of attr3_sub is
begin
  y <= not a;
end;

library ieee;
use ieee.std_logic_1164.all;

package attr3_pkg is
  component attr3_sub is
    port (
      a : in  std_logic;
      y : out std_logic);
  end component;
  attribute test : string;
  attribute test of attr3_sub: component is "yes";
end;
  
library ieee;
use ieee.std_logic_1164.all;
use work.attr3_pkg.all;

entity attr3 is port (
  a : in  std_logic;
  y : out std_logic);
end entity;

architecture beh of attr3 is
begin
   b0 : attr3_sub port map (a, y);
end beh;
