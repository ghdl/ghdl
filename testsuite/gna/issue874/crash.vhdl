library ieee;
use     ieee.std_logic_1164.all;
use     ieee.math_real.all;

entity testbench is
    generic(
       c_clock_mhz :real    := 66.0
    );
end entity;

architecture rtl of testbench is
    constant c_ns_clock     :real    := (1.0/c_clock_mhz)*1000.0;
    constant c_ns_write     :real    := 10.0;
    constant c_ns_read      :real    := 25.0;

begin

-- synthesis translate_off
process 
begin
    report "sram_ctrl2   c_ns_clock    :" & to_string(c_ns_clock, "%0.2f");
    report "sram_ctrl2   c_ns_write    :" & to_string(c_ns_write, "%0.2f");
    report "sram_ctrl2   c_ns_read     :" & to_string(c_ns_read, "%0.2f");
	wait;
end process;
-- synthesis translate_on

end architecture;  
