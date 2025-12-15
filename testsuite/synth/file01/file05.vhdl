library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity file05 is
end;

architecture behaviour of file05 is
    type ram_t is array(0 to 1023) of std_logic_vector(7 downto 0);

    impure function init_ram(name : STRING) return ram_t is
        file ram_file : text;
        variable l : line;
        variable tmp : ram_t := (others => (others => '0'));
    begin
      file_open(ram_file, name, read_mode);
      assert not endfile(ram_file);

        return tmp;
    end function;

    signal memory : ram_t := init_ram("STD_OUTPUT");
begin
end architecture behaviour;
