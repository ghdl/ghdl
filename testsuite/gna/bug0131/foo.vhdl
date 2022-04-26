library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo is
end entity;

architecture fum of foo is
    signal counter:         unsigned (6 downto 0) := (others => '0');
begin
    process (counter) is
        constant DIV_FACTOR_FM_C:   unsigned (6 downto 0) := (others => '1');
        CONSTANT A : unsigned(6 DOWNTO 0) := "0010100";  -- 20
        CONSTANT B : unsigned(6 DOWNTO 0) := DIV_FACTOR_FM_C - 1;
    begin
        CASE counter IS
           WHEN A =>
           WHEN B =>
           WHEN OTHERS =>
         END CASE;
     end process;
end architecture;
