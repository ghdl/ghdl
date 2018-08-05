entity e1 is end entity;
architecture a of e1 is
  type t is range 0 to 1;
  constant c :t := 7 - 6;
begin
  assert c = 3 report "c /= 3" severity note;
  assert c = t(3) report "c /= 3" severity note;
end architecture;
