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
    check(b"1010", "1010");
    check(b"1Z-", "1Z-");
    check(b"a", "a");
    check(o"17", "001111");
    check(o"7U", "111UUU");
    check(o"A", "AAA");
    check(x"A5", "10100101");
    check(x"XZ", "XXXXZZZZ");
    check(x"G", "GGGG");
    check(d"160", "10100000");
    check(d"9", "1001");
    wait;
  end process;
end;
