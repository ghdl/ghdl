library ieee;
use ieee.std_logic_1164.all;

entity BugTest is
end entity BugTest;

architecture rtl of BugTest is
	constant voltageChangeRegLengthC : integer := 52;

    -- Window signals
    signal rxRightBoundxDP, rxRightBoundxDN : integer range 0 to voltageChangeRegLengthC := 0;
    signal rxLeftBoundxDP, rxLeftBoundxDN   : integer range 0 to voltageChangeRegLengthC := 7;

    signal voltageChangeIntervalxDI : std_logic_vector(voltageChangeRegLengthC-1 downto 0);
    signal txDataxDN : std_logic_vector(7 downto 0);
	
    constant paddingNumber : natural := voltageChangeRegLengthC mod 8;
    constant paddingVector : std_logic_vector(7 - paddingNumber downto 0) := (others => '0');
    

begin
    
    -- Problematic part of the code
    txDataxDN <= paddingVector
            & voltageChangeIntervalxDI(rxLeftBoundxDP downto rxRightBoundxDP)
            when (rxLeftBoundxDP - rxRightBoundxDP) < 7 else 
            voltageChangeIntervalxDI(rxLeftBoundxDP downto rxRightBoundxDP);

end architecture rtl;
