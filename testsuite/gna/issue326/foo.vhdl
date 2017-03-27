entity foo is
	port(a, b: out bit);
end entity foo;
architecture bug of foo is
begin
	a <= '0', '1' after 1 ns, '0' after 2 ns, '1' after 3 ns;
	process(a)
	begin
		b <= not a;
	end process;
end architecture bug;
