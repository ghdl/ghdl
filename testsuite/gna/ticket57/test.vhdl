use std.textio.all;

entity test is
end entity;

architecture a of test is
  type file_t is file of character;
  function fun(var : boolean) return boolean is
    file f : file_t;
    variable l : line;
  begin
    file_open(f, "filename.txt", write_mode);
    write(f, character'( 'x' ));
    write(f, LF);
    file_close(f);
    return var;
  end function;
begin
  main : process
    constant c : boolean := fun(false);
  begin
    wait;
  end process;
end architecture;
