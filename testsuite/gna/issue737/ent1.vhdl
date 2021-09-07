entity ent1 is
end entity;

architecture impl of ent1 is
	type bitvv is array(natural range <>) of bit_vector;
	signal foo: bitvv(2 downto 0)(0 downto 0);
        signal foo1 : bitvv(1 downto 0)(0 downto 0);
begin

foo1 <= foo(1 downto 0);

end architecture;
