entity pb is
end pb;

architecture behav of pb is
  type t is record
    v : integer;
  end record;

  function "-" (a, b : t) return t is
    variable v : integer;
  begin
    "-".v := a.v;
    v := "-".b.v;
    return b;
  end "-";
begin
end;
