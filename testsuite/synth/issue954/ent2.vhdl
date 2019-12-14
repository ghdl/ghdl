entity ent is
    port (
        i : in bit;
        i2 : bit;
        o : out bit
    );
end ent;

architecture a of ent is
begin
  o <= i or i2;
end;

