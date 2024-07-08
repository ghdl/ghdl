package pkg_rec is
  generic (type g_type);
  type t_rec is record
    el : g_type;
  end record;
end;

entity tst_rec is
end;

architecture sim of tst_rec is
  package StringPackage is new work.pkg_rec
                             generic map (G_TYPE => string);
  signal s : StringPackage.t_rec;
begin
end architecture sim;
