entity sens is
end entity sens;

architecture rtl of sens is
  type rec_t is record
    a : bit_vector(3 downto 0);
    b : bit;
  end record;
  signal r : rec_t;
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
end architecture rtl;
