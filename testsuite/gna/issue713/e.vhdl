entity e is
end entity;

architecture a of e is
	function foo(n : positive) return bit_vector is
	begin
		return (n downto 0 => '0');
	end function;
begin
	process
	begin
		for i in foo(3)'range loop
			report integer'image(i);
		end loop;
		
		for i in foo(2)'reverse_range loop
			report integer'image(i + foo(4)'length);
		end loop;
		
		wait; --forever
	end process;
end architecture;
