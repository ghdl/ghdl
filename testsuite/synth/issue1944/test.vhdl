package gen_pkg is
   generic (type T);
end gen_pkg;

entity test is
end entity test;

architecture simple of test is
   package pkg is new work.gen_pkg generic map (t => integer);
begin
end architecture simple;
