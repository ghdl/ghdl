package testp is
  generic (type G_KEY_TYPE);
  type t_dict_iter is record
    key : G_KEY_TYPE;
  end record t_dict_iter;
end package testp;

package body testp is
end package body testp;

entity test is
end entity test;

architecture sim of test is
  -- bug
  package StringPackage is new work.testp
    generic map (G_KEY_TYPE => string);
  -- works
  package NaturalPackage is new work.testp
    generic map (G_KEY_TYPE => natural);
begin
end architecture sim;
