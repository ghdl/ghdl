library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
	port (
		dummy : in std_ulogic
	);
end bug;

architecture struct of bug is
  type table_t is array (natural range<>, natural range<>) of natural;

  function fun return table_t is
    variable ret : table_t(0 to 1, 0 to 3);
  begin
    ret(0,0) := 100;
    ret(0,1) := 101;
    ret(0,2) := 102;
    ret(0,3) := 103;
    ret(1,0) := 110;
    ret(1,1) := 111;
    ret(1,2) := 112;
    ret(1,3) := 113;
    for i in ret'range(1) loop
      for j in ret'range(2) loop
        report "t("& integer'image(i) & "," & integer'image(j)
          & ")=" & integer'image(ret(i,j));
      end loop;
    end loop;
    return ret;
  end function;

  constant table : table_t := fun;
begin
	gen_i : for i in table'range(1) generate
		gen_j : for j in table'range(2) generate
			b : block is
				function print return std_ulogic is
				begin
					report "index="& integer'image(i) & "," & integer'image(j) & "; " &
					       "length="& integer'image(table'length(1)) & "," & integer'image(table'length(2));
					return '0';
				end function;

				constant tmp : std_ulogic := print;
				constant entry : natural := table(i, j);
			begin

			end block;
		end generate;
	end generate;
end architecture;
