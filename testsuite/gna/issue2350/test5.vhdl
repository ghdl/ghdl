library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;

entity test5 is
end entity ;

architecture arch of test5 is

    type cx_t is record
        re : signed (15 downto 0);
        im : signed (15 downto 0);
    end record ;

--    subtype c16_t is cx_t( re(15 downto 0), im(15 downto 0) ) ;

    constant a : signed(16 downto 0) := "00000001000000010"; --(others => '0');
    constant x : cx_t := ( re => to_signed(0, 16), im => a);

begin
end architecture ;
