entity test is
end entity test;

architecture rtl of test is

  type test_t is record
    t1 : natural;
    t2 : natural;
  end record test_t;

  constant C_TEST_T : test_t :=
    (
      t1 => 1,
      t2 => 2);

  constant C_TEST : bit_vector(0 to 7) :=
    (C_TEST_T.t1 => '1', others => '0');

begin

end architecture rtl;
