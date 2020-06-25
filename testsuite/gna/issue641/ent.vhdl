-- Helper package
library ieee;
use     ieee.std_logic_1164.all;

package p is
	type BusRecord is record
		Address : std_logic_vector;
		Data    : std_logic_vector;
	end record;
end package;

-- DUT
library ieee;
use     ieee.std_logic_1164.all;
use     work.p.all;

entity e is
	port (
		Axi : inout BusRecord
	);
end entity;

architecture a of e is
	alias  AxiData is Axi.Data;

	signal Address : std_logic_vector(Axi.Address'range);
	signal Data1   : AxiData'subtype;    -- line 27: declaration of signal "data1" with unconstrained array subtype "std_logic_vector" is not allowed
	signal Data2   : Axi.Data'subtype;   -- line 28: prefix must denote an object; a type mark must be a simple or expanded name
begin

end architecture;

-- Top Level
library ieee;
use     ieee.std_logic_1164.all;
use     work.p.all;

entity test is
end entity;

architecture tb of test is
	signal AxiBus : BusRecord(
		Address(7 downto 0),
		Data(31 downto 0)
	);
begin
	DUT: entity work.e
		port map (
			Axi => AxiBus
		);
end architecture;

