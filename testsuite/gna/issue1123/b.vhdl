library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity A is
port(x: in std_ulogic_vector(4 downto 0));
end entity;

architecture test of A is
begin
end architecture;

entity B is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

architecture test of B is
    function to_vector(signal d: unsigned(4 downto 0)) return std_ulogic_vector is
    begin
        return std_ulogic_vector(d);
    end function;

    signal s: unsigned(4 downto 0) := (others => '0');
begin
    test: entity work.A
    port map(
        x => to_vector(s)
    );
end architecture;
