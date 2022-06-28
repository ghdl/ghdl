entity ent is
end entity;

architecture a of ent is
begin
  process
    variable b : boolean;
    variable l : std.textio.line;
  begin
    b := false;
    std.textio.write(l, b);
    report l.all & " should be false";
    l := null;
    b := true;
    std.textio.write(l, b);
    report l.all & " should be true";
    wait;
  end process;
end;
