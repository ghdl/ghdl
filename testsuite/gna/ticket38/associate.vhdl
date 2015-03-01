
entity test_ent is
    port (
        input: integer 
    ); 
end entity;

architecture test of test_ent is
    
begin

end architecture;

entity associate is
end entity;

architecture test of associate is
    component test_ent is
        port (
            input:  integer
        );
    end component;
begin
gen_label:
    for i in 0 to 11 generate
    genx:
        test_ent
            port map (
            input => i
            );
    end generate;
end architecture;
