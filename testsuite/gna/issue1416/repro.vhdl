entity repro is
  port (a : bit := '0';
        o : out bit);
end;

architecture behav of repro is
  signal s : bit;
begin
  s <= a;
  o <= not s;

  process
  begin
    wait for 1 ns;
    assert a = '0' and s = '0' severity failure;
    s <= force '1';
    assert s = '0' severity failure;
    wait for 0 ns;
    assert s'active severity failure;
    assert a = '0' and s = '1' severity failure;
    wait for 1 ns;
    s <= release;
    wait for 0 ns;
    assert s'active severity failure;
    assert a = '0' and s = '0' severity failure;
    wait;
  end process;
end;
