entity e is end entity;
architecture a of e is
  component c is
    generic(constant k :natural := 0);
    port   (signal   s :bit_vector(k to k));
  end component;
begin
  inst: c port map(s(k) => '0');
end architecture;
