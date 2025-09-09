entity rec_assoc is
  generic (l : natural := 3);
end entity;

architecture arch of rec_assoc is
  type rec_t is record
    a, b : bit_vector;
  end record;

    signal x : bit_vector(0 to l - 1) := (0 => '0', others => '1');
    signal y : bit_vector(0 to 1) := "10";
begin
    b0 : block is
        port (m : in rec_t);
        port map (m.a => x, m.b => y);
    begin
        p0 : process is
        begin
            wait for 1 ns;
            assert m.a = "011";
            assert m.b = "10";
            wait;
        end process;
    end block;
end architecture;
