library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo is
    constant m: integer := 3; 
    constant n: integer := 5; 
    constant h: integer := 4; 
    constant DATA_SIZE: integer :=5;
end entity;

architecture fum of foo is
    signal OUTPUT : SIGNED((DATA_SIZE+DATA_SIZE)+(m-1)-1 downto 0) := "000011110110" ;

     type Vector is record
            OUTPUT_test : SIGNED((DATA_SIZE+DATA_SIZE)+(m-1)-1 downto 0);
     end record;

    type VectorArray is array (natural range <>) of Vector;

    constant Vectors : VectorArray := (
         -- Values to be compaired to calculated output
        (OUTPUT_test =>"000011110110"), -- 246  (CORRECT)
        (OUTPUT_test =>"000101001000")  -- 382  (INCORRECT)        
        );

begin
TEST:
    process 
    begin
        for i in Vectors'RANGE loop
            assert OUTPUT = Vectors(i).OUTPUT_test
            report "Incorrect Output on vector line " & integer'image(i) &
--            lf & "Expected:" & integer'image(i)(to_integer((Vectors(i).OUTPUT_test)))
            lf & "Expected:" & integer'image(to_integer((Vectors(i).OUTPUT_test)))
            severity error;
        end loop;
        wait;
    end process;

end architecture;
