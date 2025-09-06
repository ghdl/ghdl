entity recmat_aggr is end entity;

architecture arch of recmat_aggr is
  type matrix_t is array (natural range <>) of bit_vector;
  type rec_t is record
    a, b : matrix_t;
  end record;

  signal x1 : bit_vector(0 to 2) := "010";
  signal x2 : bit_vector(0 to 2) := "110";
  signal y : bit_vector(0 to 1) := "10";
begin
    b0 : block is
        port (m : in rec_t);
        port map (m => ((x1, x2), (1 => y)));
    begin
        p0 : process is
        begin
            wait for 1 ns;
            assert m.a(0) = "010";
            assert m.a(1) = "110";
            assert m.b(1) = "10";
            wait;
        end process;
    end block;
end architecture;
