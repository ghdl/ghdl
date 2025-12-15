library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity file07 is
end;

architecture behaviour of file07 is
    impure function init_ram(name : STRING) return boolean is
        type int_file is file of integer;
        file ram_file : int_file;
    begin
      flush(ram_file);
      return true;
    end function;

    signal memory : boolean := init_ram("STD_OUTPUT");
begin
end architecture behaviour;
