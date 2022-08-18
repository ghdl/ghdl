package pkg is
	type rec is record
		elem : bit_vector;
	end record;
end package;

entity i is
	port (
		signal p : out work.pkg.rec
	);
end entity;

architecture a of i is
	signal s1 : work.pkg.rec(elem(3 downto 0));
	signal s2 : s1'subtype;
begin
	s1.elem <= (others => '0');
	s2.elem <= (others => '0');
--	p.elem <= (p.elem'range => '0');
	p.elem <= (others => '0');        -- reproducer.vhdl:20:19: 'others' choice not allowed for an aggregate in this context
end architecture;

entity e is
end entity;

architecture a of e is
	signal s : work.pkg.rec(
		elem(7 downto 0)
	);
begin
	inst : entity work.i
		port map (
			p => s
		);
end architecture;
