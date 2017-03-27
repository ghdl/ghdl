entity bar is
	port(a, b: out bit);
end entity bar;
architecture gub of bar is
begin
	a <= '0', '1' after 1 ns, '0' after 2 ns, '1' after 3 ns;
	process(all)
	begin
		b <= not a;
	end process;
end architecture gub;
