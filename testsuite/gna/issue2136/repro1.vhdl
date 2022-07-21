entity repro1 is
end;

architecture BHV of repro1 is
  type matrixType is array(natural range <>) of bit_vector;

  function get_min( a : matrixType) return bit_vector is
    variable res    : a'element;
  begin
    res:=a(a'left);
    return res;
  end function get_min;

  signal matrix1 : matrixType(0 to 1)(7 downto 0):=(x"80",x"10");
begin

  process
    variable min1    : bit_vector  (7 downto 0);
  begin
    min1 := get_min(matrix1);
    assert min1 = x"80" severity failure;
    wait;
  end process;
end;
