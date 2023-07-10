entity t3 is
end entity;

architecture arch of t3 is
    constant A : positive := 3;
    constant B : positive := 4;
begin
    assert A * +5 /= 0;
end architecture;
