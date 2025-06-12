entity repro1 is
end;

architecture behav of repro1 is
  function f1(a : integer) return integer
  is
    alias as : integer is a;
  begin
    return as;
  end;

  function f2(a : integer) return integer
  is
    alias as : integer is a;
  begin
    return f1(as);
  end;
begin
  assert f1(5) = 5;
  assert f2(7) = 7;
end behav;
