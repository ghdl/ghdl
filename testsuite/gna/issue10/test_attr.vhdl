entity test_attr is
end entity;

architecture rtl of test_attr is
  type T_TUPLE is record
    A  : NATURAL;
    B  : NATURAL;
  end record;
  type T_VECTOR is array (NATURAL range <>) of T_TUPLE;

  attribute attr : t_vector;
  attribute attr of t_tuple : type is ((8, 32), (8, 20), (8, 36));
begin
  genTests : for i in t_tuple'attr'range generate
    constant LOCAL_A : NATURAL := t_tuple'attr(i).A;
    constant LOCAL_B : NATURAL := t_tuple'attr(i).B;
  begin
    -- my tests
  end generate;
end architecture;
