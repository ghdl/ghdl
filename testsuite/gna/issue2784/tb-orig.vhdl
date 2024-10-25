library ieee;                                                                   
use ieee.std_logic_1164.all;                                                    
use ieee.numeric_std.all;                                       
use std.textio.all;                                           
                        
entity testbench is
end entity;

architecture a of testbench is
    type vector_array is array(natural range <>) of std_ulogic_vector;          
    type vector_array_array is array(natural range <>) of vector_array;         
    subtype phy_data_t is vector_array(63 downto 0)(7 downto 0);                
    subtype phy_edc_t is vector_array(7 downto 0)(7 downto 0);                  
                                                                                
    signal clk : std_ulogic := '0';                                             
    signal data : phy_data_t;                                                   
--     signal edc : phy_edc_t;                                                  
                                                                                
    constant test_pattern : vector_array_array(open)(0 to 8)(7 downto 0) := (   
        (X"00", X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"00"),        
        (X"00", X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"FF"),        
        (X"00", X"FF", X"00", X"00", X"00", X"00", X"00", X"00", X"02")         
    );                                                                          
                                                                                
    subtype TEST_RANGE is natural range test_pattern'RANGE;                     
    signal test_index : TEST_RANGE := test_pattern'LOW;                         
                                                                                
begin                                                                           
    clk <= not clk after 5 ns;        
    process (clk)                                                               
        variable linebuffer : line;                                             
    begin                                                                       
        if rising_edge(clk) then                                                
            if test_index < test_pattern'HIGH then                              
                test_index <= test_index + 1;                                   
            else                                                                
                test_index <= test_pattern'LOW;                                 
            end if;                                                             

            write(linebuffer, to_hstring(test_pattern(test_index)(8)));     
            writeline(output, linebuffer);   
        end if;                                                                 
    end process;
end;
