entity repro13 is
end;

architecture behav of repro13 is
  type my_arr is array (natural range <>) of bit_vector;

  procedure assign (signal v : inout my_arr; d : my_arr) is
  begin
    v (d'length - 1 downto 0) <= d;
  end assign;
  signal s1 : my_arr (2 downto 0)(3 downto 0);
begin
  process
    variable v0 : my_arr (1 downto 0)(3 downto 0);
  begin
    v0 := (x"1", x"2");
    s1 <= (x"5", x"4", x"3");
    wait for 1 ns;
    assign (s1, v0);
    wait for 1 ns;
    assert s1(2) = x"5";
    assert s1(1) = x"1";
    assert s1(0) = x"2";
    wait;
  end process;
end behav;
