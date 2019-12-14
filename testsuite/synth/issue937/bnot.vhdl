entity bnot is
    port (
        i : in bit;
        o : out bit
    );
end entity;

architecture a of bnot is
begin
  o <= not i;
end;

