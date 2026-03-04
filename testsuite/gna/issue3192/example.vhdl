library ieee;
use ieee.std_logic_1164.all;

entity element_range_example is
end entity;

architecture rtl of element_range_example is

-- A 2D array type: array of std_logic_vector
type slv_array_t is array (natural range <>) of std_logic_vector(7 downto 0);

-- Procedure using val'element'range (TRIGGERS GHDL BUG)
procedure buggy_proc (
    signal val : in slv_array_t
) is
begin
    for i in val'element'range loop   -- ERROR: constant interface "val" was
        null;                          -- not annotated with attribute "element"
    end loop;
end procedure;

-- Procedure using val(val'left)'range (WORKAROUND - works in GHDL)
procedure fixed_proc (
    signal val : in slv_array_t
) is
begin
    for i in val(val'left)'range loop  -- OK: indexes first element's range
        null;
    end loop;
end procedure;

signal my_array : slv_array_t(0 to 3);  -- 4 x 8-bit elements


    
  

begin

process is
begin
    my_array(0) <= x"AA";
    my_array(1) <= x"BB";
    my_array(2) <= x"CC";
    my_array(3) <= x"DD";
    wait for 10 ns;

    fixed_proc(my_array);  -- loops over bits 7 downto 0 ✓
    buggy_proc(my_array);  -- ERROR in GHDL ✗

    wait;
end process;


    
  

end architecture;
