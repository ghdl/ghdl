entity tb_tc4 is
end tb_tc4;

architecture behav of tb_tc4 is
  signal state : bit;
  signal o     : bit_vector(3 downto 0);
begin
  dut: entity work.tc4
    port map (state, o);

  process
  begin
    state <= '0';
    wait for 1 ns;
    assert o = "0111" severity failure;

    state <= '1';
    wait for 1 ns;
    assert o = "1000" severity failure;

    wait;
  end process;
end behav;
