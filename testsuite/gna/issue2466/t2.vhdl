entity t2 is
end entity;

architecture arch of t2 is
    constant A : positive := 3;
    constant B : positive := 4;
begin
    assert A * abs 5 /= 0;
end architecture;
