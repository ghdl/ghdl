library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.env.all;

entity tbMux is
end entity tbMux;

architecture Bhv of tbMux is
	constant cWaitTime : time := 5 ns;
	
	signal iA_ulogic, iB_ulogic, oRes_ulogic : std_ulogic;
	signal iA_natural, iB_natural, oRes_natural : natural;

	type aCustomType is (PossibleValueA, PossibleValueB, PossibleValueC, PossibleValueD);
	signal iA_custom, iB_custom, oRes_custom : aCustomType;

	signal iSel : std_ulogic;
begin
	Mux_ulogic : entity work.GenericMux
		generic map(gaMuxType => std_ulogic)
		port map(
			iA   => iA_ulogic,
			iB   => iB_ulogic,
			oRes => oRes_ulogic,
			iSel => iSel
		);
		
	Mux_Natural : entity work.GenericMux
		generic map(gaMuxType => natural)
		port map(
			iA   => iA_natural,
			iB   => iB_natural,
			oRes => oRes_natural,
			iSel => iSel
		);
	
		
	stimul : process is
	begin
		iA_ulogic <= '0';
		iB_ulogic <= '1';
		
		iA_natural <= 123;
		iB_natural <= 456;
		

		iSel <= '0';

		wait for cWaitTime;

		assert oRes_ulogic = iA_ulogic report "wrong value passed through ulogic-mux" severity failure;
		assert oRes_natural = iA_natural report "wrong value passed through natural-mux" severity failure;
		assert oRes_custom = iA_custom report "wrong value passed through custom-type-mux" severity failure;

		iSel <= '1';

		wait for cWaitTime;

		assert oRes_ulogic = iB_ulogic report "wrong value passed through ulogic-mux" severity failure;
		assert oRes_natural = iB_natural report "wrong value passed through natural-mux" severity failure;

		finish;
	end process;
end architecture Bhv;
