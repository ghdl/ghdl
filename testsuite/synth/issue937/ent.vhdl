entity ent is
    port (
        i : in bit;
        o : out bit
    );
end entity;

architecture a of ent is
    signal x : boolean;
begin
    process(i)
    begin
        if not x then
            o <= i;
        else
            o <= '0';
        end if;
    end process;
end;
