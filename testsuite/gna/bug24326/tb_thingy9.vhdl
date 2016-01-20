entity tb_thingy is
end tb_thingy;

architecture tb of tb_thingy is
	component thingy is
		generic (
			a_a : integer
		);
		port (
			x%x : in bit; -- <==
			y_y : out bit
		);
	end component;
	signal stimuli : bit;
	signal response : bit;
begin

	dut : thingy
	generic map (
		a_a => 42
	)
	port map (
		x_x => stimuli,
		y_y => response
	);

end tb;
