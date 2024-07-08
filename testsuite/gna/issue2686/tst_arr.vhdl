package pkg_arr is
  generic (type g_type);
  type t_arr is array (natural range <>) of g_type;
end;

entity tst_arr is
end;

architecture sim of tst_arr is
  package StringPackage is new work.pkg_arr
                             generic map (G_TYPE => string);
  signal s : StringPackage.t_arr(1 to 2);
begin
end architecture sim;
