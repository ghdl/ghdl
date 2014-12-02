package attributes_pkg is
	attribute period :time;
end package;


library work;
use work.attributes_pkg.period;
entity inner is
	port(
		signal clk :in bit
	);
end entity;
architecture arch of inner is
	constant CLK_PERIOD :time := clk'period;
begin
end architecture;


library work;
use work.attributes_pkg.period;
entity outer is end entity;
architecture arch of outer is
	signal clk :bit;
	attribute period of clk :signal is 1 ns;
begin
	inst: entity work.inner port map(clk);
end architecture;
