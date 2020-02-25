library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_ghdl_test is
end entity tb_ghdl_test;

architecture tb of tb_ghdl_test is
begin
  p_test : process is
    file f_in : text;
    variable v_line     : line;
  begin
    file_open(f_in, "ghdl_test.txt", read_mode);

    while not endfile(f_in) loop
      readline(f_in, v_line);
      report v_line.all;
    end loop;

    wait;
  end process;
end architecture;
