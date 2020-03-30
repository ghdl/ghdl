library ieee;
use ieee.std_logic_1164.all;

entity issue2 is
    port (foo : in  std_logic;
          bar : out boolean);
end issue2;

architecture behav of issue2 is
begin

  --bar <= true when (?? foo) else false; -- works
    bar <= true when false xor (?? foo) else false;

end architecture;
