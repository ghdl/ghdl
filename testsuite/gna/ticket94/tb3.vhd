
library ieee;
use ieee.std_logic_1164.all;
library alib;

entity tb3 is
end;

architecture arch of tb3 is

    signal a, b :  std_logic := '0';

begin
    ainst: alib.apkg.acomp
        port map (a, b);

    process is
    begin
        a <= '0';
        wait for 1 ns;
        assert b = '0' report "component is missing" severity failure;
        a <= '1';
        wait for 1 ns;
        assert b = '1' report "component is missing" severity failure;
        wait;
  end process;

end architecture;
