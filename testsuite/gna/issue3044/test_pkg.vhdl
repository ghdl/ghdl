library ieee;
    -- use ieee.numeric_std.all;
    use ieee.numeric_bit.all;

package test_pkg is
    
    function test(nx : natural) return unsigned;

    function test(nx : natural) return signed;
    
end package;

package body test_pkg is
  
    function test(nx : natural) return unsigned is
        variable start    : unsigned(nx-1 downto 0);
        variable temp1    : unsigned(nx-1 downto 0); 
        variable temp2    : unsigned(nx-1 downto 0); 
        variable temp3    : unsigned(nx-1 downto 0); 
        variable temp4    : unsigned(nx-1 downto 0); 
        variable temp5    : unsigned(nx-1 downto 0); 
        variable temp6    : unsigned(nx-1 downto 0); 
        variable temp7    : unsigned(nx-1 downto 0); 
        variable temp8    : unsigned(nx-1 downto 0); 
    begin
        start := to_unsigned(10, nx);
        temp1 := resize(start, nx);
        temp2 := resize(temp1, nx);
        temp3 := resize(temp2 + 2, nx);
        temp4 := resize(temp3 - 1, nx);
        temp5 := resize(temp4 * 4, nx);
        temp6 := resize(temp5 / 2, nx);
        temp7 := resize(temp6 mod 15, nx);
        temp8 := resize(temp7 rem 11, nx);
        return temp8;
    end function;
    
    function test(nx : natural) return signed is
        variable start    : signed(nx-1 downto 0);
        variable temp1    : signed(nx-1 downto 0); 
        variable temp2    : signed(nx-1 downto 0); 
        variable temp3    : signed(nx-1 downto 0); 
        variable temp4    : signed(nx-1 downto 0); 
        variable temp5    : signed(nx-1 downto 0); 
        variable temp6    : signed(nx-1 downto 0); 
        variable temp7    : signed(nx-1 downto 0); 
        variable temp8    : signed(nx-1 downto 0); 
    begin
        start := to_signed(-10, nx);
        temp1 := resize(abs(start), nx);
        temp2 := resize(-temp1, nx);
        temp3 := resize(temp2 + 2, nx);
        temp4 := resize(temp3 - 1, nx);
        temp5 := resize(temp4 * 4, nx);
        temp6 := resize(temp5 / 2, nx);
        temp7 := resize(temp6 mod 15, nx);
        temp8 := resize(temp7 rem 11, nx);
        return temp8;
    end function;
    
end package body test_pkg;
