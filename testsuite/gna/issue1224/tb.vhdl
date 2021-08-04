library ieee;
use ieee.std_logic_1164.all;

entity tb is
end;

architecture arch of tb is

  type ulogic_mat_t is array(natural range <>) of std_logic_vector; --this requires vhdl08

begin
  process
    constant logic2D: ulogic_mat_t(0 to 1) := (('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-'), ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-'));
  begin

    for i in logic2D'range loop
      for j in logic2D'range(1) loop
        report "2D [" & integer'image(i) & "," & integer'image(j) & "]: " & std_logic'image(logic2D(i)(j)) severity note;
      end loop ;
    end loop ;

    wait;
  end process;
end;
