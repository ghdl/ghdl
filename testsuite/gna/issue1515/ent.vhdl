entity ent is
    port (
        inp  : BIT_VECTOR(1 downto 0);
        outp : buffer BOOLEAN
    );
end;

architecture a of ent is
begin
    outp <= inp'delayed(1 ns)(0) = '0';
end;
