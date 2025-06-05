entity demo2 is
end entity;

architecture tb of demo2 is
  type t_unsigned_vector is array (natural range <>) of bit_vector;
  type t_unsigned_array is array (natural range <>) of t_unsigned_vector;

  procedure test_procedure_2(range_vec : t_unsigned_array) is
  begin
    report "Executing test_procedure_2";
  end procedure;
begin
  test_procedure_2((0 => (x"0", x"3"), 1 => (x"c", x"f0")));
end architecture tb;
