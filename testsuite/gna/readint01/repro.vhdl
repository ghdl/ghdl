use std.textio.all;

entity repro is
end entity;

architecture test of repro is
  --  A sign without digits is not an integer: READ must fail and leave
  --  the line unchanged.
  procedure check_bad (s : string) is
    variable l : line := new string'(s);
    variable value : integer;
    variable good : boolean;
  begin
    read (l, value, good);
    assert not good
      report "INTEGER READ accepted """ & s & """" severity failure;
    assert l.all = s
      report "INTEGER READ changed the line for """ & s & """"
      severity failure;
    deallocate (l);
  end check_bad;

  procedure check_good (s : string; expected : integer; rest : string) is
    variable l : line := new string'(s);
    variable value : integer;
    variable good : boolean;
  begin
    read (l, value, good);
    assert good and value = expected and l.all = rest
      report "bad INTEGER READ for """ & s & """" severity failure;
    deallocate (l);
  end check_good;
begin
  process
  begin
    check_bad ("-");
    check_bad ("+");
    check_bad ("  -");
    check_bad (" +");
    check_bad ("- ");
    check_bad ("+ 7");
    check_bad ("-x");

    check_good ("-7", -7, "");
    check_good ("+7", 7, "");
    check_good ("7", 7, "");
    check_good ("  -7 tail", -7, " tail");
    check_good ("+7 tail", 7, " tail");
    check_good ("-0", 0, "");
    wait;
  end process;
end;
