entity tb_leftof01 is
end tb_leftof01;

architecture behav of tb_leftof01 is
  signal i, o : integer := 9;
begin
  dut: entity work.leftof01
    port map (i, o);

  process
  begin
    i <= 5;
    wait for 1 ns;
    assert o = 4 severity failure;
    wait;
  end process;
end behav;
