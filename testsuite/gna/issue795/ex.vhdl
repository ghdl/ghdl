library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.csv_file_reader_pkg.all;
-- csv file should be in format
-- sync period,v1hexvalue,v2integervalue,v3hexvalue,comment
-- eg 123445,AD45,234,F00D,"test line, for crash example"
--
entity crashExample is
  port (
  	vctrFlNm	: in string;
  	fEnd		: out  boolean; 
  	syncCount 	: out integer;
  	syncPulse 	: out std_logic;
  	v1 			: out std_logic_vector(15 downto 0);
  	v2			: out std_logic_vector(15 downto 0);
  	v3			: out std_logic_vector(15 downto 0);
  	vctrCmt		: out string
  );
end entity crashExample;

architecture BEHAVIORAL of crashExample is
	signal commentString 		: string(1 to LINE_LENGTH_MAX);
	signal syncCntInt 			: integer;
	constant SYN_PLSE_PRD		: time := 4000 ns; 
begin
	
	-- comment map
	vctrCmt <= commentString;

	-- Vector Process
	prcs_VctrRead : process
		variable csv			: csv_file_reader_type;
		variable syncPeriod		: time;
	begin
		-- initialse end flag
		fEnd <= false;
		-- open file
		csv.initialize(vctrFlNm);
		-- read first line of file which should be a comment	
		csv.readline;
		-- initalise variables/signals
		syncCntInt <= 0;
		-- while (not end of file)
		while not csv.end_of_file loop
			-- read information until next SYN command is found
			csv.readline;
			-- read Sync period
			syncPeriod := csv.read_integer * 1 ns;
			-- read Hex integer
			v1 <= std_logic_vector(to_unsigned(csv.read_hex, v1'length));
			-- read normal integer
			v2 <= std_logic_vector(to_unsigned(csv.read_integer, v2'length));
			-- read Hex integer
			v3 <= std_logic_vector(to_unsigned(csv.read_hex, v3'length));
			-- read String comment
			commentString <= csv.read_string;
			-- Make sync pulse
			syncPulse <= '1';
			syncCount <= syncCntInt;
			wait for SYN_PLSE_PRD;
			syncPulse <= '0';
			-- wait for the sync period
			wait for (syncPeriod - SYN_PLSE_PRD);
			syncCntInt <= syncCntInt + 1;
		end loop;
		fEnd <= true;
		wait; -- end simulation
	end process prcs_VctrRead;
	
end BEHAVIORAL;
