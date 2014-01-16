entity top is
end top;

use std.textio.all;

architecture ARCH of TOP is

  type int_vector is array (integer range<>) of integer;
  function driver_counter( values : int_vector ) return integer is
    variable result : integer := 1;
    variable l: line;
  begin
    for index in values'range loop
      if values(index) /= 0 then
        result := result + values (index);
        write (l, integer'image(values(index)) & ",");
      end if;
    end loop;
    report l.all & " count resolved => " & integer'image(result);
    return result;
  end function;

  signal S1: driver_counter integer := 6;

begin
  s1 <= 1 after 1 ns;

  check: process
  begin
    assert s1 = 7 report "resolution function not called at init"
      severity failure;
    wait for 1 ns;
    assert s1 = 2 report "resolution function not called at 1 ns"
      severity failure;
    wait;
  end process;
end architecture;
