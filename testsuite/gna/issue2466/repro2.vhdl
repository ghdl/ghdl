entity repro2 is
end entity;

architecture arch of repro2 is
    constant A : positive := 3;
    constant B : positive := 4;
    constant C : boolean := (2**A)**5 >= 0;
begin

end architecture;
