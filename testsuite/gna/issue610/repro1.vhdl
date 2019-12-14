entity repro1 is
   port (d : bit_vector := x"01");
end repro1;


architecture behav of repro1
is
  type t_bv_array is array (natural range <>) of bit_vector;
begin
  process
    variable v : t_bv_array (0 to 0)(d'length - 1 downto 0);
  begin
    v(0) := d;
    assert v(0)(0) = '1' severity failure;
    assert v(0)(1) = '0' severity failure;
    wait;
  end process;
end behav;
