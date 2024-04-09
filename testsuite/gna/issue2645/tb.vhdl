library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity example_tb is
end entity;

architecture rtl of example_tb is
begin

  p_main : process
    file file_handler  : text;
    variable v_line    : line;
    variable v_boolean : boolean := True;
  begin
    file_open(file_handler, "output.txt", write_mode);
    write(v_line, v_boolean);
    writeline(file_handler, v_line);
    file_close(file_handler);
    v_line := null;

    file_open(file_handler, "output.txt", read_mode);
    readline(file_handler, v_line);
    read(v_line, v_boolean);
    file_close(file_handler);

    wait;
  end process p_main;

end architecture rtl;
