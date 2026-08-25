use std.textio.all;

entity pure_reprint is
  port (o : out bit);
end pure_reprint;

architecture a of pure_reprint is
  signal s1 : bit := '0';
  signal s2 : bit := '1';

  --  Declared pure, but reads two signals of the enclosing architecture,
  --  and declares a file as well.  All three violations must be reported.
  pure function f return bit is
    constant c : bit := s1;
    file fp : text;
  begin
    return c xor s2;
  end f;
begin
  o <= f;
end a;
