entity ent is
end entity;

architecture impl of ent is
  type bool_vector is array(0 downto 0) of boolean;
  signal baz: bool_vector;
begin

assert baz(0)
  severity note;

end architecture;
