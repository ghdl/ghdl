library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;

entity test2 is
end entity ;

architecture arch of test2 is

    type cx_t is record
        re : signed (15 downto 0);
        im : signed (15 downto 0);
    end record ;

--    subtype c16_t is cx_t( re(15 downto 0), im(15 downto 0) ) ;

    constant x : cx_t := ( re => to_signed(0, 16), im => to_signed(0, 17) ) ;

begin
end architecture ;
