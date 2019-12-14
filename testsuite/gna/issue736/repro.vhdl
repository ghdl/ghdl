entity repro is
end repro;

architecture behav of repro is
  signal a, i, r : bit;
begin
  process (all)
  begin
    r <= a when i = '0' else not a;
  end process;

  process
  begin
    i <= '0';
    a <= '1';
    wait for 1 ns;
    assert r = '1' severity failure;

    i <= '0';
    a <= '0';
    wait for 1 ns;
    assert r = '0' severity failure;

    i <= '1';
    a <= '1';
    wait for 1 ns;
    assert r = '0' severity failure;

    wait;
  end process;
end behav;
