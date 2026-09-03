use work.pack_RC_Add_n_F.all;

--  The design of ent.vhdl with the formal designator written ascending,
--  which is the direction LRM08 5.3.2.2 e) 2) gives the unconstrained
--  formal A.  The actual stays 'downto': an association only has to match
--  on length.
entity RC_Add_n_F_ok is
    generic(n : natural := 4);
    port(A, B : in bit_vector(n-1 downto 0); Cin: in bit; Sum: out bit_vector(n-1 downto 0); Cout: out bit);
end RC_Add_n_F_ok;

architecture Arch_RC_Add_n_F_ok of RC_Add_n_F_ok is
    signal result: bit_vector(n downto 0);
begin
    result <= RC_Add_n(A(0 to 3) => A(3 downto 0), B => B, Cin => Cin);
    Sum <= result(n-1 downto 0);
    Cout <= result(n);
end Arch_RC_Add_n_F_ok;
