entity err_lit01 is
  generic (m : natural := 5);
end;

architecture behav of err_lit01 is
  signal s : natural range 0 to m := 10;
begin
  assert s < 7;
end;
