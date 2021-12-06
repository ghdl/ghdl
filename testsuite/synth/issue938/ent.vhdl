entity ent is
    port (
        r : in bit;
        s : in bit;
        q : out bit
    );
end entity;

architecture a of ent is
begin
    process(r, s)
    begin
        if r = '1' then
            q <= '0';
        elsif s = '1' then
            q <= '1';
        end if;
    end process;
end;

