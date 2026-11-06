entity test_rec_idx is
end entity;

architecture rtl of test_rec_idx is
  type t_record is record
    a : bit_vector(3 downto 0);
    b : bit;
  end record;
  signal r : t_record;
  signal y : bit;
  signal s : bit_vector(1 downto 0);
begin
  p_index : process (r.a)
  begin
    y <= r.a(0);
  end process;

  p_slice : process (r.a)
  begin
    s <= r.a(1 downto 0);
  end process;
end architecture;
