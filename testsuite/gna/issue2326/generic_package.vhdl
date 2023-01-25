package generic_package is
   generic (type t);
   subtype st is t;
end;

entity testbench is
end;

architecture sim of testbench is
   package gp is new work.generic_package
      generic map (t => bit_vector(1 downto 0));

   constant c : gp.st := "10";
begin
   test : process begin
      report to_string(c);
      wait;
   end process;
end;
