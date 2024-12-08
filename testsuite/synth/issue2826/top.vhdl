package defs is
    type rec is record
        a: integer range 0 to 1;
        b: integer range 0 to 0;
        c: integer range 0 to 1;
    end record;
end package;

entity top_level is
    port (
        o: out work.defs.rec
    );
end entity;

architecture arch of top_level is
begin
    o <= (
        a => 0,
        b => 0,
        c => 0
    );
end architecture;
