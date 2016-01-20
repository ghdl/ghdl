entity tb_thingy is
end tb_thingy;

architecture tb of tb_thingy is
	component thingy is
		port (
			x_x : in bit;
			y_y : out bit
		);
	end component;
	signal stimuli : bit;
	signal response : bit;
begin

	dut : thingy
	port map (
		x-x => stimuli, -- <== spelling error
		y_y => response
	);

end tb;
