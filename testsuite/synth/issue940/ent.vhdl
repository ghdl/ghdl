entity ent is
    port (
        i : in bit_vector(3 downto 0);
        o : out bit_vector(3 downto 0)
    );
end entity;

architecture a of ent is
begin
    o <= i;
end;
