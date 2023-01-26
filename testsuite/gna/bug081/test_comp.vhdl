package test_comp is
  function log2 (v : natural) return natural;

  component my_comp is
    generic (max : natural);
    port (o : out bit_vector(log2(max) - 1 downto 0));
  end component;
end test_comp;
