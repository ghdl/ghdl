library ieee;
use ieee.std_logic_1164.all;

entity foo is
end entity;
architecture fum of foo is
    constant A: std_logic_vector (7 downto 0) := X"04";
    
    function slv_image(inp: std_logic_vector) return string is
        variable image_str: string (1 to inp'length);
        alias input_str:  std_logic_vector (1 to inp'length) is inp;
    begin
        for i in input_str'range loop
            image_str(i) := character'VALUE(std_ulogic'IMAGE(input_str(i)));
        end loop;
        return image_str;
    end;
        
begin
SOME_LABEL:
    process
    begin
        wait for 1 ns;
        if A <= "00001011" then -- if A <= std_logic_vector'("00001011") then
            report "A = " & slv_image(A) ;
        end if;
        wait;
    end process;
end architecture;
