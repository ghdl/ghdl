package gen_pkg is
   generic (type T);

   procedure proc;
end gen_pkg;

package body gen_pkg is
    procedure proc is
    begin
        null;
    end proc;
end gen_pkg;

entity test is
end entity test;

architecture simple of test is
   package pkg is new work.gen_pkg generic map (t => integer);
begin
end architecture simple;
