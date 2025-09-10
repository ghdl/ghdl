use std.textio.all;
use std.env.finish;

entity A is
end A;

architecture sim of A is
begin
  process
    procedure b is
      variable c : line;
    begin
      write(c, to_string(now));
      writeline(output, c);
      deallocate(c);
    end procedure;
  begin
    b;
    wait for 10 ns;
    b;
    finish;
  end process;
end architecture;
