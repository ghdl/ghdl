-- Port with constrained array of unconstrained elements.
entity ent is end entity;

architecture arch of ent is
  type my_rec is record
    v : bit_vector;
    c : character;
  end record;
  
  type matrix_t is array (natural range <>) of my_rec;
  signal x : bit_vector(0 to 1) := "01";
  signal y : bit_vector(0 to 1) := "10";
begin
  b0 : block is
    port (m : in matrix_t(0 to 1));
    port map (m(0).v => ("01"), m(0).c => 'A', m(1).v => y, m(1).c => 'V');
  begin
    p0 : process is
    begin
      wait for 1 ns;
      assert m(0).v = "01";
      assert m(1).v = "10";
      wait;
    end process;
  end block;
end architecture;
