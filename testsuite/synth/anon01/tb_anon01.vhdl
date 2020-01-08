entity tb_anon01 is
end tb_anon01;

architecture behav of tb_anon01 is
  signal i, o : bit_vector(6 downto 0);
begin
  dut: entity work.anon01
    port map (i, o);

  process
  begin
    i <= b"000_0000";
    wait for 1 ns;
    assert o = b"010_0101" severity failure;

    i <= b"111_1111";
    wait for 1 ns;
    assert o = b"101_1010" severity failure;

    wait;
  end process;
end behav;
