library ieee;
use ieee.std_logic_1164.all;
use std.env.finish;


entity test is
end entity test;

architecture rtl of test is
    
    type std_logic_vector_array is array(natural range <>) of std_logic_vector;
    signal test_sig : std_logic_vector_array(0 to 3)(7 downto 0) := (others => (others => '0'));

begin
    
    Test_Proc: process
    begin
        for i in 0 to 15 loop
            test_sig <= test_sig(1 to 3) & std_logic_vector'(x"FF");
            wait for 1 us;
            report "TESTING" severity note;
        end loop;    
        finish;
    end process;

end architecture rtl;