entity repro3 is
end;

architecture arch of repro3 is
  type integer_acc is access integer;
  type my_rec is record
    nat : natural;
    p : integer_acc;
  end record;

  impure function t return my_rec is
  begin
    return (1, null);
  end t;
begin
  assert t.nat = 1 severity failure;
end;
