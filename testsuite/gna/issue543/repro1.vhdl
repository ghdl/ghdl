entity repro1 is
end entity;

architecture arch of repro1 is
  attribute att : integer_vector;
  constant const : integer := 1;
  attribute att of const: constant is (2, 3);
  constant const2 : integer_vector := const'att;
  constant const3 : integer_vector := const2;
begin
  assert false
    report "const'att(0) is " & integer'image(const3(0))
    severity note;
end architecture;
