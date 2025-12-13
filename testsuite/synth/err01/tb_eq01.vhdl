entity tb_eq01 is
end;

architecture behav of tb_eq01 is
  signal a : bit;
  signal r : boolean;
begin
  dut: entity work.eq01
    port map (a, r);

  process
  begin
    a <= '0';
    wait for 1 ns;
    assert r severity failure;

    a <= '1';
    wait for 1 ns;
    assert r severity failure;

    wait;
  end process;
end behav;
