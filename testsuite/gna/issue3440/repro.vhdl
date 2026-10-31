entity repro is
end entity;

architecture a of repro is
  procedure check(s : string; res : string) is
  begin
    assert s = res report "bad expansion: " & s & ", expected " & res
      severity failure;
  end check;
begin
  process
  begin
    check(0x"0", "");
    check(0sb"", "");
    check(1sx"0", "0");
    check(1sx"F", "1");
    check(2sx"E", "10");
    wait;
  end process;
end;
