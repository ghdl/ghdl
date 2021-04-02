entity e1 is
	port (
		vector : in  bit_vector(3 downto 0);
		output : out bit
	);
end entity;

architecture a1 of e1 is
	signal zero : vector'element;
begin
	zero   <= vector(0);

	process(all) is
		variable z : vector'element;
	begin
		z := zero;

		output <= z;
	end process;
end architecture;


entity e1_tb is
end entity;

architecture top of e1_tb is
    constant c1 : bit_vector(3 downto 0) := (others => '0');
    signal vector : bit_vector(3 downto 0);
    alias vector_type : bit_vector(1 downto 0) is vector(1 downto 0);
    signal output : vector_type'element;
begin
	l1: entity work.e1
		port map (
			vector => vector,
			output => output
		);

    tb_proc: process
        variable v1 : bit_vector(3 downto 0);
        variable v2 : v1'element;
        variable v3 : c1'element;
    begin
        v1 := (others => '0');
        vector <= v1;
        wait for 1 ns;
        v1 := (others => '1');
        vector <= v1;
        wait for 1 ns;
        v1 := (others => '0');
        vector <= v1;
        wait for 1 ns;
        wait;
    end process;

end architecture;
