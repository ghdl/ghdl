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

architecture fum of assemble is
    
    type std_logic_2d is array 
         (integer range <>, integer range <>) of std_logic;
    
    type column is array (31 downto 0) of std_logic;
    type row is array (2 downto 0) of std_logic;
    
    signal data:    std_logic_2d (column'range, row'range);
    
    function to_std_logic_2d  (i0,i1,i2: std_logic_vector (column'range)) 
    return std_logic_2d is 
        variable retdat: std_logic_2d (column'range, row'range); 
    begin 
        for i in column'range loop
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

architecture fie of assemble is
    
    type std_logic_2d is array 
         (integer range <>, integer range <>) of std_logic;
    
    subtype column is std_logic_vector (31 downto 0);
    subtype row is std_logic_vector (2 downto 0);
    
    signal data:    std_logic_2d (column'range, row'range);
    
    function to_std_logic_2d  (i0,i1,i2: std_logic_vector (column'range)) 
    return std_logic_2d is 
        variable retdat: std_logic_2d (column'range, row'range); 
    begin 
        for i in retdat'range(1) loop
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

