entity e2 is
end entity;

architecture a of e2 is
  function get_left return natural is
  begin
    return 12;
  end get_left;

  constant l : natural := get_left;
  subtype t is integer range 0 to l;
  signal a : t;
begin
  assert FALSE report natural'image(a'subtype'left) severity NOTE;

  assert FALSE report a'subtype'image(a) severity NOTE;
end architecture;
