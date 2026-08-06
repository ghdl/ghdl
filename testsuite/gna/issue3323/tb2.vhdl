entity tb2 is
end;

architecture arch of tb2 is
begin
  process
    procedure chk(val : time; unit: time; res : string)
    is
      constant img : string := to_string(val, unit);
    begin
      assert img = res report "got: " & img & ", expect: " & res
        severity failure;
    end chk;
  begin

    chk(1 min, sec, "60 sec");
    chk(-1 min, sec, "-60 sec");
    chk(5436 ns, ps, "5436000 ps");
    chk(5436 ps, ns, "5.436 ns");
    chk(1 hr, ms, "3600000 ms");

    chk(1 hr, sec, "3600 sec");
    chk(-1 hr, min, "-60 min");
    chk(60 min, hr, "1 hr");
    chk(90 min, hr, "1.5 hr");
    chk(70 min, hr, "1.16666666666666666666 hr");
    wait;
  end process;
end;

