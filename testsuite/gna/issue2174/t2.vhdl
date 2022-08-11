use std.textio.all;

entity t2 is
end;

architecture behav of t2 is
  subtype stext is text;

  procedure w (file f : stext; s : string) is
  begin
    write (f, s);
  end w;
begin
  process
  begin
    w (output, "hello" & LF);
    wait;
  end process;
end behav;
