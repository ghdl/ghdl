use work.pkg.all;

entity tb is 
end entity tb;

architecture arch of tb is
begin
	process
	begin 
		report integer'image(c_int.get);
		wait;
	end process;
end arch ;
