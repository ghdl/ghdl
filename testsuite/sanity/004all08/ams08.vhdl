entity vibration is
end;

architecture behav of vibration is
  subtype displacement is real tolerance "def_disp";
  nature electrical is real across real through ref reference;
  quantity x1 : real;
  terminal v0, v1 : electrical;
--  quantity vd1 across id1, ic1 through v0 to v1;
  quantity vd2 := 5.2 across v0 to v1;
  quantity vd3 through v0 to v1;
begin
  x1 == 3.5;
end behav;
