library ieee;
    -- use ieee.numeric_std.all;
    use ieee.numeric_bit.all;

library work;
    use work.test_pkg;

entity test_tb is

end test_tb;

architecture tb of test_tb is
-- #######################
-- # Comment these lines in/out to see the problem!
-- #######################
    constant unsigned_x_c : unsigned(7 downto 0) := work.test_pkg.test(8);
    constant signed_x_c   : signed(7 downto 0)   := work.test_pkg.test(8);
begin
    
    p_main : process
        variable unsigned_x : unsigned(7 downto 0);
        variable signed_x   : signed(7 downto 0);
    begin
        
        unsigned_x := work.test_pkg.test(8);
        report "unsigned_x = " & natural'image(to_integer(unsigned_x));
        
        signed_x := work.test_pkg.test(8);
        report "signed_x = " & natural'image(to_integer(signed_x));
        
        wait;
    end process;
    
end architecture tb;
