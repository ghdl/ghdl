library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity file03 is
    generic(RAM_INIT_FILE : string := "firmware.hex");
end;

--  Call endfile on a non-open file.

architecture behaviour of file03 is
    type ram_t is array(0 to 1023) of std_logic_vector(7 downto 0);

    impure function init_ram(name : STRING) return ram_t is
        file ram_file : text open read_mode is name;
        variable l : line;
        variable tmp : ram_t := (others => (others => '0'));
    begin
      assert not endfile(ram_file);

        return tmp;
    end function;

    signal memory : ram_t := init_ram(RAM_INIT_FILE);
begin
end architecture behaviour;
