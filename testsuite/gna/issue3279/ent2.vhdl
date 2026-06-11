entity ent is
end entity;

architecture test of ent is
    type mwe_prot_t is protected
        procedure init (bv : bit_vector);
    end protected;

    type mwe_prot_t is protected body
        procedure init (bv : bit_vector) is
        begin
        end procedure;
    end protected body;
begin
    process
    begin
        wait;
    end process;
end architecture;
