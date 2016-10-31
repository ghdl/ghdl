package p is
  component c is
    generic (
      -- None of these work in GHDL 1a1d378dcafeca5a18dfa8862ebe412efa1e9718
      -- together with the ports defined below.
--      g : bit_vector
--      g : bit_vector := x"0"
--      g : bit_vector(3 downto 0) := x"0"
      g : bit_vector(3 downto 0)
    );
    port (
      -- fails if generic 'g' is referenced
      x : bit_vector(g'length-1 downto 0)
      );
  end component;
end package;
