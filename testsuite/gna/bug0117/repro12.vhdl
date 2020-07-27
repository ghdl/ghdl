entity repro12 is
end;

architecture behav of repro12 is
  type my_arr is array (natural range <>) of bit_vector;

  procedure assign (v : inout my_arr; d : my_arr) is
  begin
    v (d'length - 1 downto 0) := d;
  end assign;
begin
  process
    variable v1 : my_arr (2 downto 0)(3 downto 0);
    variable v0 : my_arr (1 downto 0)(3 downto 0);
  begin
    v0 := (x"1", x"2");
    v1 := (x"5", x"4", x"3");
    assign (v1, v0);
    assert v1(2) = x"5";
    assert v1(1) = x"1";
    assert v1(0) = x"2";
    wait;
  end process;
end behav;
