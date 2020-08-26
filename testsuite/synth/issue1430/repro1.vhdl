entity repro is
  generic (a : natural := 5);
end;

architecture behav of repro is
  constant c : natural := 10;
begin
  assert false
    report natural'image(a) & "c = " & integer'image(c)
    & ", a = " & natural'image(a) & " (!)" severity note;
end;
