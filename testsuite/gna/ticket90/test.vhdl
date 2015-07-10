entity test is
    generic (
    a, b : integer);
begin
    assert a = b report "a /= b" severity failure;
end entity;

architecture a of test is
begin
end architecture;
