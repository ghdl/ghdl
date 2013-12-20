entity foo is
end foo;

use std.textio.all;

architecture only of foo is
  signal clock : bit;
begin  -- only
  process (clock)
    variable x : integer := 0;
    variable l : line;
  begin  -- process
    write( l, string'( "x = " ) );
    write( l, x );
    writeline( output, l );
    x := x + 1;
  end process;

  process
  begin  -- process
    clock <= '1' after 1 ns,
             '0' after 2 ns,
             '1' after 3 ns;
    wait;
  end process;
end only;
