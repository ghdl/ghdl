library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity phony is
	port (
		i : in  std_logic;
		o : out std_logic
	);
end entity;

architecture synth of phony is

	constant BLOCKS : natural := 5000;
	constant BITS   : natural := 5;

	-- Unused but necessary to reproduce the crash : passing a vector is necessary
	function func1(A : std_logic_vector) return natural is
	begin
		return 1;
	end function;

	function func2(b : boolean) return natural is
	begin
		return func1(std_logic_vector(to_unsigned(0, BITS)));
	end function;

	-- Unused but necessary to reproduce the crash
	constant x : natural := func2(true);

	component LUT is
		port (
			O  : out std_logic;
			I0 : in  std_logic
		);
	end component;

begin

	gen : for b in 0 to BLOCKS-1 generate

		-- Both of these, lut and process, can reproduce the crash

		l : LUT
			port map (
				O  => open,
				I0 => '0'
			);

		p : process(all)
			variable v : natural;
		begin
			v := BITS;
		end process;

	end generate;

	o <= i;

end architecture;


library ieee;
use ieee.std_logic_1164.all;

library work;
use work.all;

entity engine is
	generic (
		SIZE : natural := 10
	);
	port (
		i : in std_logic;
		o : out std_logic
	);
end entity;

architecture synth of engine is

begin

	gen : for c in 0 to SIZE-1 generate

		ph : entity phony
			port map (
				i => '0',
				o => open
			);

	end generate;

	o <= i;

end architecture;

