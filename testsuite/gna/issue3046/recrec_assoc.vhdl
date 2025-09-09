entity recrec_assoc is end entity;

architecture arch of recrec_assoc is
  type matrix_t is array (natural range <>) of bit_vector;
  type rec1_t is record
    a1, b1 : bit_vector;
  end record;

  type rec2_t is record
    a2 : rec1_t;
    b2 : bit_vector;
  end record;

  signal xa1 : bit_vector(0 to 2) := "010";
  signal xa2 : bit_vector(0 to 2) := "110";
  signal y : bit_vector(0 to 1) := "10";
begin
    b0 : block is
        port (m : in rec2_t);
        port map (m.a2.a1 => xa1, m.a2.b1 => xa2, m.b2 => y);
    begin
        p0 : process is
        begin
            wait for 1 ns;
            assert m.a2.a1 = "010";
            assert m.a2.b1 = "110";
            assert m.b2 = "10";
            wait;
        end process;
    end block;
end architecture;
