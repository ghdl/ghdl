library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;

entity test is
end entity ;

architecture arch of test is

    type cx_t is record
        re : signed ;
        im : signed ;
    end record ;

    subtype c16_t is cx_t( re(15 downto 0), im(15 downto 0) ) ;

    constant x : c16_t := ( re => to_signed(0, 16), im => to_signed(0, 17) ) ;

begin

    tb : process
    begin
        std.env.stop ;
    end process ;

end architecture ;
