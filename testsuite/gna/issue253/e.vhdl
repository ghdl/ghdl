entity e is
end entity;

architecture a of e is
  signal a : bit;
  constant b : bit := '0';
begin
  assert FALSE report bit'image(a'subtype'left) severity NOTE;
  assert FALSE report bit'image(b'subtype'left) severity NOTE;

  assert FALSE report a'subtype'image(a) severity NOTE;
  assert FALSE report b'subtype'image(b) severity NOTE;
end architecture;
