entity test is
end test;

architecture arch of test is
  type natural_vec is array (natural range <>) of natural;
  type natural_vec_ptr is access natural_vec;
  procedure bad is
    variable v : natural_vec_ptr;
  begin
    v := new natural_vec_ptr(0 to 9); -- Should give an error, gives assertion failed
    v := new natural_vec(0 to 9); -- The correct syntax for the above (which works fine)
  end procedure;
begin
end arch;

