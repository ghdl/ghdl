entity mwe1 is
end entity;

architecture test of mwe1 is
    type rec_t is record
        a : integer;
        b : integer;
        c : integer;
    end record;
begin
    process
        variable rec1 : rec_t;
    begin
      for i in 0 to 3 loop
        rec1 := rec_t'(character'pos('('), i, -2);

        report integer'image(rec1.a);
        report integer'image(rec1.b);
        report integer'image(rec1.c);

        assert rec1.a = character'pos('(');
        assert rec1.b = i;
        assert rec1.c = -2;
      end loop;
      wait;
    end process;
end architecture;
