entity repro1 is
end;

architecture behaviour of repro1 is
  function gi (vector:bit_vector) return boolean is
    subtype ur is natural range vector'length-1 downto vector'length/2;
  begin
    return vector(ur)/=(ur => '0');
  end function;
  constant c : bit_vector(3 downto 0) := "0100";
begin
  assert gi (c);
end behaviour;
