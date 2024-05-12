library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

entity tb is
end tb;

architecture sim of tb is
    
    type SlvArray_t is array(integer range<>) of std_logic_vector;
    type UnsignedArray_t is array(integer range<>) of unsigned;
    
    constant a : UnsignedArray_t(0 to 0)(0 downto 0) := (others => (others => '0'));
    constant b : SlvArray_t := SlvArray_t(a);
    
begin
    
    process
    begin
        std.env.stop;
    end process;
    
end sim;
