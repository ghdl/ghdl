entity tb_pred01 is
end tb_pred01;

architecture behav of tb_pred01 is
  signal i, o : integer := 9;
begin
  dut: entity work.pred01
    port map (i, o);

  process
  begin
    i <= 5;
    wait for 1 ns;
    assert o = 4 severity failure;
    wait;
  end process;
end behav;
