library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity file06 is
end;

architecture behaviour of file06 is
    impure function init_ram(name : STRING) return boolean is
        type int_file is file of integer;
        file ram_file : int_file;
        variable s : file_open_status;
    begin
      file_open(s, ram_file, name, read_mode);
      return s = mode_error;
    end function;

    signal memory : boolean := init_ram("STD_OUTPUT");
begin
end architecture behaviour;
