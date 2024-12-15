package defs is
    type rec is record
        a: integer range 0 to 1;
        b: integer range 0 to 0;
        c: integer range 0 to 1;
    end record;
end package;

entity gen_out is
    port (
        o: out work.defs.rec
    );
end entity;

architecture arch of gen_out is
begin
    o <= (0, 0, 0);
end architecture;

entity take_in is
    port (
        i: in work.defs.rec
    );
end entity;

architecture arch of take_in is
begin
end architecture;

entity top_level is
    port (
        o: out work.defs.rec
    );
end entity;

architecture arch of top_level is
    signal sig: work.defs.rec;
begin
    inst1: entity work.gen_out
    port map (
        o => sig
    );

    inst2: entity work.take_in
    port map (
        i => sig
    );

    o <= sig;
end architecture;
