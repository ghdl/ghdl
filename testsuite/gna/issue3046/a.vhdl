-- Port with constrained array of unconstrained elements.
entity ent is end entity;

architecture arch of ent is
    type matrix_t is array (natural range <>) of bit_vector;
    signal x : bit_vector(0 to 1) := "01";
    signal y : bit_vector(0 to 1) := "10";
    signal n : matrix_t(0 to 1)(0 to 1);
begin
    n <= (x, y);

    b0 : block is
        port (m : in matrix_t(0 to 1));
        port map (m => n);
    begin
        p0 : process is
        begin
            wait for 1 ns;
            assert m(0) = "01";
            assert m(1) = "10";
            wait;
        end process;
    end block;
end architecture;
