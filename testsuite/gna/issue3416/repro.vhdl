entity repro is
end entity;

architecture a of repro is
  procedure check(t : time; res : string) is
    constant s : string := to_string(t, ns);
  begin
    assert s = res report "bad result for to_string(" & time'image(t)
      & ", ns): " & s severity failure;
  end check;
begin
  process
  begin
    check (0 ns, "0 ns");
    check (10 ns, "10 ns");
    check (100 ns, "100 ns");
    check (1000 ns, "1000 ns");
    check (19 ns, "19 ns");
    check (110 ns, "110 ns");
    check (-10 ns, "-10 ns");
    wait;
  end process;
end;
