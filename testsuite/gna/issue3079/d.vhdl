entity D is
end D;

architecture sim of D is

  function f return natural is
  begin
    return 8;
  end f;

  constant b_t_bits : integer := f;
  subtype b_t is bit_vector(b_t_bits - 1 downto 0);

  signal c : b_t := (others => '0');
  type b_arr_t is array (natural range <>) of b_t;

begin
  process
    procedure p(e : b_arr_t) is
    begin
      assert e'left = 0;
      assert e'right = 1;
      assert e'length = 2;
      for i in e'range loop
        report natural'image(i);
        c <= e(i);
        wait for 10 ns;
      end loop;
    end procedure;
  begin
    p((
      x"00", 
      x"01"
    ));
    wait;
  end process;
end architecture;
