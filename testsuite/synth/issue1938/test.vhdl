package gen_pkg is
   generic (N : integer);
end gen_pkg;

entity test is
end entity test;

architecture simple of test is
   package pks is new work.gen_pkg generic map (N => 1);
begin
end architecture simple;
