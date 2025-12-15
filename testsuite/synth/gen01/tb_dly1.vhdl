entity tb_dly1 is
end tb_dly1;

architecture behav of tb_dly1 is
  signal a, r : bit;
begin
  dut: entity work.dly1
    port map (a, r);

  process
  begin
    a <= '1';
    wait for 1 ns;
    assert r = '1' severity failure;
    
    a <= '0';
    wait for 1 ns;
    assert r = '0' severity failure;
    
    wait;
  end process;
end behav;
