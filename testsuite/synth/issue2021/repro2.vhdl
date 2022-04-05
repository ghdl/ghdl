entity repro2 is
end;

architecture ent of repro2 is
  type my_record is record
    field_a : bit_vector;                           -- Parametrized on instantiation
    field_b : bit_vector;
  end record;

  subtype my_record1 is my_record (field_b(3 downto 0));

  signal bar : my_record1(field_a(7 downto 0));
  signal baz : my_record1(field_a(7 downto 0));

begin

end ent;
