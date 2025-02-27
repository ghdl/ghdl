-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic multi-FF synchronizer.
--
library ieee;
use     ieee.std_logic_1164.all;


-- Multi-stage FF synchronizer
entity sync_Bits is
	generic (
		BITS      : positive              := 1;
		STAGES    : positive range 2 to 5 := 3
	);
	port (
		Clock     : in  std_logic;

		Input     : in  std_logic_vector(BITS - 1 downto 0);
		output    : in  std_logic_vector(BITS - 1 downto 0)
	);
end entity;


architecture rtl of sync_Bits is
	component sync_Bit is
		generic (
			STAGES    : positive range 2 to 5 := 3
		);
		port (
			Clock     : in  std_logic;

			Input     : in  std_logic;
			output    : in  std_logic
		);
	end component;
begin
	gen : for i in Input'range generate
	begin
--		bit: component work.sync_Bit
		bit: entity work.sync_Bit
			generic map (
				STAGES => STAGES
			)
			port map (
				Clock => Clock,
				Input => Input(i),
				Input => Output(i)
			);
	end generate;
end architecture;


-- Single-Bit Multi-stage FF synchronizer
entity sync_Bit is
	generic (
		STAGES    : positive range 2 to 5 := 3
	);
	port (
		Clock     : in  std_logic;

		Input     : in  std_logic;
		output    : in  std_logic
	);
end entity;


architecture rtl of sync_Bit is
	signal meta   : std_logic := '0';
	signal ffs    : std_logic_vector(STAGES - 1 downto 1) := (others => '0');
begin
	meta   <= Input when rising_edge(Clock);
	ffs    <= (ffs(ffs'left downto 1) & meta) when rising_edge(Clock);

	Output <= ffs(ffs'left);
end architecture;
