entity ent is
end ent;

architecture ent of ent is
  type my_record is record
    field_a : bit_vector;                           -- Parametrized on instantiation
    field_b : bit_vector(31 downto 0); -- Width set by generic
  end record;

  signal bar : my_record(field_a(7 downto 0));
  signal baz : my_record(field_a(7 downto 0));

begin

end ent;
