entity tb_assert2 is
end tb_assert2;

architecture behav of tb_assert2 is
  signal v, res : natural;
  signal en : boolean := false;
begin
  dut: entity work.assert2
    port map (v, en, res);

  process
  begin
    en <= True;
    v <= 2;
    wait for 1 ns;
    assert res = 3 severity failure;

    v <= 11;
    en <= False;
    wait for 1 ns;
    assert res = 0 severity failure;

--    wait for 10 ns;
--    en <= True;
    wait;
  end process;
end behav;
