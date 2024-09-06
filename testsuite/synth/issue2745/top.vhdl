library ieee;
use ieee.std_logic_1164.all;

entity top is
	generic (
		WDATA : natural := 8
	);
	port (
		top_di : in  std_logic_vector(WDATA-1 downto 0);
		top_do : out std_logic_vector(WDATA-1 downto 0)
	);
end top;

architecture synth of top is

	component comp is
		generic (
			WIN  : natural := 8;
			WOUT : natural := 8
		);
		port (
			di : in  std_logic_vector(WIN-1 downto 0);
			do : out std_logic_vector(WOUT-1 downto 0)
		);
	end component;

	signal sig_di : std_logic_vector(WDATA+2-1 downto 0) := (others => '0');

begin

	-- Partial assignment, the rest is supposed to stay to the init value
	-- Issue : This gets extended with high-impedance 'Z'
	sig_di(WDATA-1 downto 0) <= top_di;

	comp_inst : comp
	generic map (
		WIN  => WDATA+2,
		WOUT => WDATA
	)
	port map (
		di => sig_di,
		do => top_do
	);

end architecture;
