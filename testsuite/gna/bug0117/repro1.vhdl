entity repro1 is
end repro1;

architecture behav of repro1 is
  type bv_array is array(natural range <>) of bit_vector;
  type bv_array_ptr is access bv_array;

  procedure reshape (d : bv_array) is
    constant word_len : natural := d(d'low)'length;
    variable sym : bv_array_ptr;
  begin
    sym := new bv_array (0 to d'length - 1)(word_len - 1 downto 0);
    sym.all := d;
  end;

  signal s : bv_array(1 to 2)(7 downto 0);
begin
  process
  begin
    reshape(s);
    wait;
  end process;
end behav;
