library ieee;
use ieee.std_logic_1164.all;

entity assemble is
    port (
        A:  in  std_logic_vector(31 downto 0);
        B:  in  std_logic_vector(31 downto 0);
        C:  in  std_logic_vector(31 downto 0);
        F:  out std_logic_vector(31 downto 0)
    );
end entity;

architecture foo of assemble is
    
    type std_logic_2d is array 
        (integer range <>, integer range <>) of std_logic;
        
    signal data:    std_logic_2d (31 downto 0, 2 downto 0);
    
    function to_std_logic_2d  (i0,i1,i2: std_logic_vector (31 downto 0)) 
    return std_logic_2d is 
        variable retdat: std_logic_2d (data'range(0), data'range(1)); 
    begin 
        for i in i0'range loop
            retdat(i, 0) := i0(i);
            retdat(i, 1) := i1(i);
            retdat(i, 2) := i2(i);
        end loop; 
        return retdat;
    end function;
    
begin
 
     data <= to_std_logic_2d(A, B, C);
     F <= (others => data (15, 1)); -- B(15)
     
 end architecture;
