-- this is a simple example that demonstrates the usage of generic types in VHDL 2008


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity GenericMux is
	generic (type gaMuxType);
	port(
		iA		: in gaMuxType;
		iB		: in gaMuxType;
		oRes	: out gaMuxType;

		iSel	: in std_ulogic
	);
end entity GenericMux;

architecture RTL of GenericMux is
	
begin
	--formal equivalent to
	/*
	Multiplex: process(all) is
	begin
		if iSel = '0' then
			oRes <= iA;
		else
			oRes <= iB;
		end if;
	end process;
	*/
	
	oRes <= iB when iSel else iA; 
end architecture RTL;
