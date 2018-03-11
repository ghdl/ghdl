library ieee;                                                                   
use ieee.std_logic_1164.all;                                                    
                                                                                
entity test is                                                                  
    port (                                                                      
    input : boolean                                                             
    );                                                                          
end test;                                                                       
                                                                                
architecture behaviour of test is                                               
    signal foo : std_logic;                                                     
begin                                                                           
    foo <= "0" when input else "1"; -- Note: Should be '0' and '1' instead      
end behaviour;                                          
