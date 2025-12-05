entity tb_right01 is
end tb_right01;

architecture behav of tb_right01 is
  signal i, o : integer := 9;
begin
  dut: entity work.right01
    port map (i, o);

  process
  begin
    i <= 5;
    wait for 1 ns;
    assert o = 5 severity failure;

    i <= 30;
    wait for 1 ns;
    assert o = 0 severity failure;
    wait;
  end process;
end behav;
