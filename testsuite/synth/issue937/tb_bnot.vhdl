entity tb_bnot is
end tb_bnot;

architecture behav of tb_bnot is
  signal i : bit;
  signal o : bit;
begin
  dut: entity work.bnot
    port map (i, o);

  process
  begin
    i <= '0';
    wait for 1 ns;
    assert o = '1' severity failure;

    i <= '1';
    wait for 1 ns;
    assert o = '0' severity failure;

    wait;
  end process;
end behav;
