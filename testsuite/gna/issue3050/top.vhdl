entity my_top is
end entity;

architecture buggy of my_top is

	type my_type1 is array (natural range <>) of integer;
	
	component my_component is
		generic (
			MY_GENERIC   : my_type1(15 downto 0) := (others=>-1)
		);
	end component;

begin

  inst : my_component
    generic map (
      MY_GENERIC(0)           => 1,
      MY_GENERIC(15 downto 1) => (others=>-1)
    );

end architecture;


