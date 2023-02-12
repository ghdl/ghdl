library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;

entity test6 is
end entity ;

architecture arch of test6 is

    type cx_t is array (1 to 2) of signed (15 downto 0);

    constant a : signed(16 downto 0) := "00000001000000010"; --(others => '0');
    constant x : cx_t := ( 1 => to_signed(0, 16), 2 => a);

begin
end architecture ;
