use std.textio.all;

entity repro is
end entity;

architecture test of repro is
  procedure check (s : string; expected : integer; rest : string) is
    variable l : line := new string'(s);
    variable value : integer;
    variable good : boolean;
  begin
    read (l, value, good);
    assert good and value = expected and l.all = rest
      report "bad INTEGER READ with GOOD for " & s severity failure;
    deallocate (l);

    l := new string'(s);
    read (l, value);
    assert value = expected and l.all = rest
      report "bad INTEGER READ for " & s severity failure;
    deallocate (l);
  end check;
begin
  process
    constant low_image : string := integer'image (integer'low);
    constant high_image : string := integer'image (integer'high);
  begin
    check ("-012 tail", -12, " tail");
    check ("-00012 tail", -12, " tail");
    check ("-012", -12, "");
    check ("  -0012 tail", -12, " tail");
    check ("-12 tail", -12, " tail");
    check ("+012 tail", 12, " tail");
    check ("012 tail", 12, " tail");
    check ("-000 tail", 0, " tail");
    check ("+000 tail", 0, " tail");
    check ("000 tail", 0, " tail");

    --  Keep negative accumulation so INTEGER'LOW can be read.
    check (low_image & " tail", integer'low, " tail");
    check ("-0" & low_image (2 to low_image'length) & " tail",
           integer'low, " tail");
    check (high_image & " tail", integer'high, " tail");
    check ("+0" & high_image & " tail", integer'high, " tail");
    wait;
  end process;
end;
