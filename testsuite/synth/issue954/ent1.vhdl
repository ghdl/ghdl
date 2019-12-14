entity ent is
    port (
        i : in bit;
        o : out bit
    );
end ent;

architecture a of ent is
begin
  o <= i;
end;

