library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;

entity test7 is
end entity ;

architecture arch of test7 is

    type cx_t is array (1 to 2) of signed (15 downto 0);

    constant x : cx_t := ( 1 => to_signed(0, 16), 2 => to_signed(0, 17));

begin
end architecture ;
