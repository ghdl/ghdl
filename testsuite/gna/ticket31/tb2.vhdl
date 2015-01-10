entity Tb2 is
end;

architecture top of Tb2 is
  function get_m15 return real is
  begin
    return -1.5;
  end get_m15;
  constant int_2     : INTEGER  := natural(get_m15);
begin
  assert FALSE report "17 - int_2 (natural(-1.5)): " & INTEGER'image(int_2) severity note;
end;
