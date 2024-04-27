library ieee;
use ieee.std_logic_1164.all;
 
entity func is
    port (
        data_i : in std_ulogic_vector;
        data_o : out std_ulogic_vector
    );
end;
 
architecture arch of func is
    function extend(data : std_ulogic_vector) return std_ulogic_vector
    is
        constant WIDTH_IN : natural := data_i'LENGTH;
        constant WIDTH_OUT : natural := data_o'LENGTH;
        variable result : std_ulogic_vector(WIDTH_OUT-1 downto 0);   
    begin
        result(WIDTH_IN-1 downto 0) := data;
        result(WIDTH_OUT-1 downto WIDTH_IN) := (others => '0');
        return result;
    end;
begin
    data_o <= extend(data_i);
end;
