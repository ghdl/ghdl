entity inc is
	generic (v : natural; vec : bit_vector);
end entity;

architecture default of inc is
begin
  assert vec'left = 0 severity failure;
  assert vec'right = v severity failure;
  assert false report integer'image(vec'right);
end architecture;

entity repro is end entity;

architecture default of repro is
begin
	g : for ix in 0 to 4 generate
	begin
		inst : entity work.inc
                  generic map (v => ix,
                               vec   => (0 to ix => '1'));
	end generate;
end architecture;
