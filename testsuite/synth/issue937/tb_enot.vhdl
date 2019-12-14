entity tb_enot is
end tb_enot;

architecture behav of tb_enot is
  signal i : bit;
  signal x : boolean;
  signal o : bit;
begin
  dut: entity work.enot
    port map (i, x, o);

  process
  begin
    i <= '0';
    x <= false;
    wait for 1 ns;
    assert o = '0' severity failure;

    i <= '1';
    x <= false;
    wait for 1 ns;
    assert o = '1' severity failure;

    i <= '1';
    x <= True;
    wait for 1 ns;
    assert o = '0' severity failure;

    wait;
  end process;
end behav;
