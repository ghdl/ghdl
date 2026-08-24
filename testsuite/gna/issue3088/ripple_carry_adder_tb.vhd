library ieee;
context ieee.ieee_std_context;

entity ripple_carry_adder_tb is
	generic (width : positive := 4);
end entity;

architecture tb of ripple_carry_adder_tb is
	signal sum : unsigned(width downto 0) := (others => '0');
begin
	UUT : entity work.ripple_carry_adder
        generic map(bit_count => width)
		port map (
            unsigned(sum) => sum(width - 1 downto 0),
            c_out => sum(width)
        );
	process
	begin
		wait for 10 ns;
		std.env.finish;
	end process;
end architecture;
