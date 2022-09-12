entity tb_rightof01 is
end tb_rightof01;

architecture behav of tb_rightof01 is
  signal i, o : integer := 9;
begin
  dut: entity work.rightof01
    port map (i, o);

  process
  begin
    i <= 5;
    wait for 1 ns;
    assert o = 6 severity failure;
    wait;
  end process;
end behav;
