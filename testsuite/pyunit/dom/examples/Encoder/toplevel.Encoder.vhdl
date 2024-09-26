-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
library IEEE;
use     IEEE.std_logic_1164.all;
use     IEEE.numeric_std.all;

library lib_Utilities;
use     lib_Utilities.Utilities_pkg.all;

use     work.StopWatch_pkg.all;


-- Toplevel module to demonstrate the translation of 4 slide-switches to 1 digit 7-segment display.
entity toplevel is
	port (
		NexysA7_GPIO_Switch         : in  std_logic_vector(3 downto 0);

		NexysA7_GPIO_Seg7_Cathode_n : out std_logic_vector(7 downto 0);
		NexysA7_GPIO_Seg7_Anode_n   : out std_logic_vector(7 downto 0)
	);
end entity;


architecture rtl of toplevel is
	signal Cathode : std_logic_vector(7 downto 0);
	signal Anode   : std_logic_vector(7 downto 0);

begin

	-- 7-segment encoder
	encoder: entity work.seg7_Encoder
		port map (
			BCDValue  => unsigned(NexysA7_GPIO_Switch),
			Dot       => '1',

			Seg7Code  => Cathode
		);

	-- convert low-active outputs
	NexysA7_GPIO_Seg7_Cathode_n <= not Cathode;
	NexysA7_GPIO_Seg7_Anode_n   <= not x"01";
end architecture;
