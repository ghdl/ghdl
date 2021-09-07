entity ent is
end entity;

architecture impl of ent is
	type bitvv is array(natural range <>) of bit_vector;
	signal foo: bitvv(2 downto 0)(0 downto 0);
begin

foo <= foo(1 downto 0) & foo(2);

end architecture;
