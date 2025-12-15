use std.textio.all;

entity file12 is
end;

architecture behaviour of file12 is

  impure function rmem(name : string) return boolean
  is
    file f : text open read_mode is "text1.txt";
    variable v : string(1 to 32);
    variable l : natural;
  begin
    --  Fails as there is no NL.
    read(f, v, l);
    return true;
  end rmem;

  constant v : boolean := rmem("intfile.bin");
begin
end architecture behaviour;
