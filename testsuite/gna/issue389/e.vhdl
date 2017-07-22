entity e is end entity;
architecture a of e is
  type enu is (x);
begin
  assert false report integer'image(x(0)) severity note;
end architecture;
