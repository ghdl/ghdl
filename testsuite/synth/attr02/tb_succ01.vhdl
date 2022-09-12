entity tb_succ01 is
end tb_succ01;

architecture behav of tb_succ01 is
  signal i, o : integer;
begin
  dut: entity work.succ01
    port map (i, o);

  process
  begin
    i <= 5;
    wait for 1 ns;
    assert o = 6 severity failure;
    wait;
  end process;
end behav;
