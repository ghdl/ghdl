entity acc2 is
end entity;

architecture test of acc2 is
  type test_t is record
    bv : bit_vector;
  end record;
  type ptr_t is access test_t;

  signal s : test_t(bv(1 downto 0));
begin
end architecture;
