entity file10 is
end;

architecture behaviour of file10 is

  impure function rmem(name : string) return boolean
  is
    type bv4_arr is array (natural range <>) of bit_vector(3 downto 0);
    type bv4_arr_file is file of bv4_arr;
    file f : bv4_arr_file open read_mode is name;
    variable v : bv4_arr(1 to 4);
    variable l : natural;
  begin
    for i in 1 to 5 loop
      read(f, v, l);
    end loop;
    return true;
  end rmem;

  constant v : boolean := rmem("bvfile.bin");
begin
end architecture behaviour;
