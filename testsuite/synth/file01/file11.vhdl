entity file11 is
end;

architecture behaviour of file11 is

  impure function rmem(name : string) return boolean
  is
    type int_file is file of integer;
    file f : int_file open read_mode is name;
    variable v : integer;
  begin
    for i in 1 to 11 loop
      read(f, v);
    end loop;
    return true;
  end rmem;

  constant v : boolean := rmem("intfile.bin");
begin
end architecture behaviour;
