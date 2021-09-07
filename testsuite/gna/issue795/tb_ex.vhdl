library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.all;
entity tbCrashExample2 is
  generic (
  	vectorFilename : string
  );
end entity tbCrashExample2;

architecture BEHAVORIAL of tbCrashExample2 is
	--! General Simulation variables/signals
	signal fVctrEnd			: boolean; 			--! Flag from vector reader to indicate file end
	signal vctrCmt			: string(1 to 256);	--! Current vector line comment
  	signal syncPulse		: std_logic;		--! Main Sync Signal
  	signal syncCount 		: integer;			--! Sync count
  	signal val1 			: std_logic_vector(15 downto 0);
  	signal val2 			: std_logic_vector(15 downto 0);
  	signal val3 			: std_logic_vector(15 downto 0);
begin
	cmp_TestVector : entity crashExample(BEHAVIORAL) 
		port map (  vctrFlNm 	=> vectorFilename,
		  			fEnd		=> fVctrEnd, 
				  	syncPulse 	=> syncPulse,
				  	syncCount 	=> syncCount,
				  	v1 			=> val1,
				  	v2			=> val2,
				  	v3			=> val3,
				  	vctrCmt		=> vctrCmt);
end BEHAVORIAL;
