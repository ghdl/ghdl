entity repro is
end entity;

architecture arch of repro is
    constant A : positive := 3;
    constant B : positive := 4;
begin
    assert (2**A)**5 >= 0;
end architecture;
