use work.pack_RC_Add_n_F.all;

entity RC_Add_n_F is
    generic(n : natural := 4);
    port(A, B : in bit_vector(n-1 downto 0); Cin: in bit; Sum: out bit_vector(n-1 downto 0); Cout: out bit);
end RC_Add_n_F;

architecture Arch_RC_Add_n_F of RC_Add_n_F is
    signal result: bit_vector(n downto 0);
begin
    -- result <= RC_Add_n(A(3 downto 0), B, Cin);                               Works
    -- result <= RC_Add_n(A => A(3 downto 0), B => B, Cin => Cin);              Works
    result <= RC_Add_n(A(3 downto 1) => A(3 downto 1), a(0) => a(0), B => B, Cin => Cin);     -- Throws exception when analyzing
 
    Sum <= result(n-1 downto 0);
    Cout <= result(n);
end Arch_RC_Add_n_F;
