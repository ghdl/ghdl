entity tb_func06 is
end tb_func06;

architecture behav of tb_func06 is
  signal a : bit_vector(3 downto 0);
  signal r : bit;
begin
  dut: entity work.func06
    port map (a, r);

  process
  begin
    a <= "0101";
    wait for 1 ns;
    assert r = '0' severity failure;

    a <= "0011";
    wait for 1 ns;
    assert r = '1' severity failure;

    wait;
  end process;
end behav;
