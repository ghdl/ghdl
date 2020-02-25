library std;
use std.textio.all;

entity count is
end entity count;

architecture tb of count is
begin
  p_test : process is
    variable v_line     : line;
    variable nlines : natural := 0;
  begin
    while not endfile(input) loop
      readline(input, v_line);
      nlines := nlines + 1;
    end loop;

    report natural'image (nlines);
    
    wait;
  end process;
end architecture;
