

entity sub1 is
end entity;

architecture test of sub1 is
    signal t : natural;
begin

    p1: process is
        alias s is <<signal ^.s : natural>>;
    begin
        s <= 5;
        wait for 0 ns;
        assert t = 42;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity sub2 is
end entity;

architecture test of sub2 is
begin

    p2: process is
        alias t is <<signal ^.u1.t : natural>>;
    begin
        t <= 42;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity ent is
end entity;

architecture test of ent is
    signal s : natural;

    component sub1 is
    end component;

    component sub2 is
    end component;
begin

    u1: component sub1;
    u2: component sub2;

    p3: process is
    begin
        assert s = 0;
        wait for 0 ns;
        assert s = 5;
        wait;
    end process;

end architecture;
