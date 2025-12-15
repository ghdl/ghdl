use std.textio.all;

entity file09 is
end;

architecture behaviour of file09 is
  constant filename : string := "text1.txt";
begin
  process
    constant msg1 : string(1 to 6) := "Hello ";
    constant msg2 : string(5 downto 1) := "World";
    file t : text;
  begin
    file_open(t, filename, write_mode);
    write(t, msg1);
    write(t, msg2);
    file_close(t);
    wait;
  end process;
end architecture behaviour;
