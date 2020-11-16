entity pass01 is
    port (
        i: in integer range -4 to 4;
        o: out integer range -4 to 4
    );
end entity;

architecture arch of pass01 is
begin
    o <= i;
end architecture;
