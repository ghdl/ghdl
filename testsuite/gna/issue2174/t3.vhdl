use std.textio.all;

entity t3 is
end;

architecture behav of t3 is
  subtype stext is text;

  procedure w (file f : text; s : string) is
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
