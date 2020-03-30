library ieee;
use ieee.std_logic_1164.all;

entity issue1 is
    port (foo : in  std_logic;
          bar : out boolean);
end issue1;

architecture behav of issue1 is
begin

    bar <= (?? foo);

end architecture;

