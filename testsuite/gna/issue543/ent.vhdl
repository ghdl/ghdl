entity ent is end entity;
architecture arch of ent is
  attribute att :integer_vector;
  constant const :integer := 1;
  attribute att of const:constant is (2, 3);
begin
  assert false
  report "const'att(0) is " & integer'image(const'att(0))
  severity note;
end architecture;
