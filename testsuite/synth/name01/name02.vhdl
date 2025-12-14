package name02_pkg is
  type arr2d is array(integer range <>, integer range <>) of bit;
end;
  
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.name02_pkg.all;

entity name02_adder is
  port (a, b : in std_logic_vector(7 downto 0);
        c : arr2d;
        r : out std_logic_vector(7 downto 0));
end name02_adder;

architecture behav of name02_adder is
begin
  r <= std_logic_vector(unsigned(a) + unsigned(b));
end behav;

library ieee;
use ieee.std_logic_1164.all;
use work.name02_pkg.all;

entity name02 is
  port (a, b : in std_logic_vector(7 downto 0);
        r : out std_logic_vector(7 downto 0));
end;

architecture behav of name02 is
  signal t : std_logic_vector(7 downto 0);
  signal c : arr2d (1 to 2, -1 to 1);
begin
  adder: entity work.name02_adder
    port map (a, b, c, t);

  r <= t and x"ee";
end behav;
