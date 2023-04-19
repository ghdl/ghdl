library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
    port (
        sel : in std_logic_vector(1 downto 0)
    );
end entity ent;

architecture arch of ent is
    signal reg : std_logic_vector(7 downto 0);
begin
    process(sel)
    begin
        reg <= (others => '0');

        -- This is the line that causes the error
        reg(to_integer(unsigned(sel)) + 1 downto to_integer(unsigned(sel))) <= (others => '1');
    end process;
end architecture arch;
