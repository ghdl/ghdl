entity mwe is
end entity;

architecture test of mwe is
    type rec_t is record
        a : integer;
        b : integer;
        c : integer;
    end record;
begin
    process
        variable rec1 : rec_t;
    begin
        rec1 := rec_t'(character'pos('('), -1, -2);

        -- character'pos('(') is equal to 40
        -- if we set rec1.a to value 40 directly, it works
        -- rec1 := rec_t'(40, -1, -2);

        report to_string(rec1.a);
        report to_string(rec1.b);
        report to_string(rec1.c);

        assert rec1.a = character'pos('(');
        assert rec1.b = -1;
        assert rec1.c = -2;

        wait;
    end process;
end architecture;
