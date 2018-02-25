package pkg1 is
  generic (
    type value_t);
end package;

package pkg2 is
  generic (
    type value_t);

  package pkg1_inst is new work.pkg1 generic map (value_t => value_t);
  use pkg1_inst.all;
end;
