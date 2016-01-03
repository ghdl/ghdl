entity test_id is
end entity;

architecture rtl of test_id is
  type T_TUPLE is record
    A  : NATURAL;
    B  : NATURAL;
  end record;
  type T_VECTOR is array (NATURAL range <>) of T_TUPLE;

  constant LIST : T_VECTOR := ((8, 32), (8, 20), (8, 36));
begin
  genTests : for i in LIST'range generate
    constant LOCAL_A : NATURAL := LIST(i).A;
    constant LOCAL_B : NATURAL := LIST(i).B;
  begin
    -- my tests
  end generate;
end architecture;
