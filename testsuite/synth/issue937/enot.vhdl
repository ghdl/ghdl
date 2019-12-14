entity enot is
    port (
        i : in bit;
        x : in boolean;
        o : out bit
    );
end entity;

architecture a of enot is
begin
    process(i, x)
    begin
        if not x then
            o <= i;
        else
            o <= '0';
        end if;
    end process;
end;

