library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tbmem is
end entity tbmem;

architecture TB of tbmem is

begin

    DM: process
        type t_ram is array(natural range <>) of unsigned;
        type p_ram is access t_ram;
        variable myram : p_ram;
    begin
        myram := new t_ram(0 to 31)(15 downto 0);
        for i in myram'range loop
            myram(i) := TO_UNSIGNED(i, 16);
        end loop;
        for i in myram'range loop
            report integer'image(i) & ": " & TO_HSTRING(myram(i));
        end loop;
        wait;
    end process DM;
    
end architecture TB;
