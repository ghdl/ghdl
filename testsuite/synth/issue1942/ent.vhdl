package gen_pkg is
   generic (N : integer);
end gen_pkg;

entity test is
end entity test;

architecture simple of test is
   package pkg is new work.gen_pkg generic map (N => 1);

   use pkg.all;
begin
end architecture simple;
