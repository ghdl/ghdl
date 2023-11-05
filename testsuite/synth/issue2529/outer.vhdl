library ieee;
use ieee.std_logic_1164.all;

entity outer is
end entity;

architecture test of outer is
begin
        inst : entity work.inner
                port map(
                        test(1) => "00",
                        test(2) => "11"
                );
end architecture;
