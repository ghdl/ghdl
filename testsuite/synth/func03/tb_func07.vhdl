entity tb_func07 is
end tb_func07;

architecture behav of tb_func07 is
  signal a, b : bit_vector(3 downto 0);
  signal r : bit;
begin
  dut: entity work.func07
    port map (a, b, r);

  process
  begin
    a <= "0101";
    b <= "0101";
    wait for 1 ns;
    assert r = '0' severity failure;

    a <= "0011";
    b <= "1101";
    wait for 1 ns;
    assert r = '1' severity failure;

    wait;
  end process;
end behav;
