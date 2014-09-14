library ieee;
use ieee.std_logic_1164.all;

package wor_std is
    subtype rddata_o_range is integer range 3 downto 0;
    type rddata_o_array is array (natural range <>) of std_logic_vector(rddata_o_range);
    
    function rddata_o_resolv (s: rddata_o_array) return std_logic_vector;   
    
    function wor_trior (s: std_logic_vector)  return std_logic;

    function slv_image(inp: std_logic_vector) return string;

end package;

package body wor_std is
        
    type wor_table is array (X01Z, X01Z) of std_ulogic;

    constant resolution_table : wor_table := (
    --      --------------------------------
    --      |  X    0    1    Z        |   |  
    --      --------------------------------
             ('X', 'X', '1', 'X'),  -- | X |
             ('X', '0', '1', '0'),  -- | 0 |
             ('1', '1', '1', '1'),  -- | 1 |
             ('X', '0', '1', 'Z')   -- | Z |
             );
             
    function wor_trior ( s: std_logic_vector ) return std_logic is
        variable result: std_logic := 'Z'; 
    begin
        if    (s'length = 1) then    return (To_X01Z(s(s'low)));
        else
            for i in s'range loop
                result := resolution_table(result, To_X01Z(s(i)));
            end loop;
        end if;
        return result;
    end wor_trior;
    
    function rddata_o_resolv (s: rddata_o_array) return std_logic_vector is
        variable wor:    std_logic_vector (s'range);
        variable result: std_logic_vector (rddata_o_range);
    begin
        for i in result'range loop
            for j in s'range loop
                wor(j) := s(j)(i);
            end loop;
            -- report "wor = " & slv_image(wor);
            result(i) := wor_trior(wor);
        end loop;
        return result;
    end function;
    
    function slv_image(inp: std_logic_vector) return string is
        variable image_str: string (1 to inp'length);
        alias input_str:  std_logic_vector (1 to inp'length) is inp;
    begin
        for i in input_str'range loop
            image_str(i) := character'VALUE(std_ulogic'IMAGE(input_str(i)));
        end loop;
        return image_str;
    end;
    
end package body;

library ieee;
use ieee.std_logic_1164.all;
use work.wor_std.all;

entity cpu_reg_dummy is
    generic ( value: std_logic_vector(3 downto 0) := (others => 'Z') );
    port (    rddata_o: out std_logic_vector(3 downto 0) );
end entity;

architecture foo of cpu_reg_dummy is 
    
begin
    rddata_o <= value after 0.5 ns;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use work.wor_std.all;

entity foe is
end entity;

architecture fum of foe is
    
    component cpu_reg_dummy
       generic ( value: std_logic_vector(rddata_o_range) := (others => 'Z') );
       port ( rddata_o: out std_logic_vector(rddata_o_range) );
    end component;
    
    signal rddata_o:  rddata_o_resolv std_logic_vector (rddata_o_range);
    
begin
    
CPU_REG1:
    cpu_reg_dummy
    generic map (value => "0000")
    port map (rddata_o => rddata_o);

CPU_REG2:
    cpu_reg_dummy
    generic map (value => "1001")
    port map (rddata_o => rddata_o);

CPU_REG3:
    cpu_reg_dummy
    generic map (value => "ZZZZ")
    port map (rddata_o => rddata_o);
    
CPU_REG4:
    cpu_reg_dummy
    generic map (value => "ZZZX")
    port map (rddata_o => rddata_o);   

WHAT:
    process
    begin
        wait for 0.6 ns;
        report "rddata_o = " & slv_image(rddata_o);
        wait;
    end process;
end architecture;
