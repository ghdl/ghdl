entity test is
    generic (
      a, b : integer;
      c : natural);
begin
  assert (a = b) and ((b /= c) or not (a = c))
    report "a /= b" severity failure;
end entity;

architecture a of test is
begin
end architecture;
