package defs is
    type rec is record
        a: integer range 0 to 1;
        b: integer range 0 to 0;
        c: integer range 0 to 1;
    end record;
end package;

entity top_sub is
    port (
        o: out work.defs.rec
    );
end entity;

architecture arch of top_sub is
begin
    o <= (
        a => 0,
        b => 0,
        c => 0
    );
end architecture;

entity top2 is
end entity;

architecture arch of top2 is
  signal s : work.defs.rec;
begin
  inst: entity work.top_sub
    port map (o => s);
end architecture;
