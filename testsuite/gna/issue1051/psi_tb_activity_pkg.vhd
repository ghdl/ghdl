------------------------------------------------------------------------------
--  Copyright (c) 2018 by Paul Scherrer Institute, Switzerland
--  All rights reserved.
--  Authors: Oliver Bruendler, Benoit Stef
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Libraries
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.psi_tb_txt_util.all;
use work.psi_tb_compare_pkg.all;

------------------------------------------------------------------------------
-- Package Header
------------------------------------------------------------------------------
package psi_tb_activity_pkg is

	-- Wait for a given time and check if the signal is idle
	procedure CheckNoActivity(signal Sig : in std_logic;
	                          IdleTime   : in time;
	                          Level      : in integer range -1 to 1; -- -1 = don't check, 0 = low, 1 = high
	                          Msg        : in string := "";
	                          Prefix     : in string := "###ERROR###: ");

	procedure CheckNoActivityStlv(signal Sig : in std_logic_vector;
	                              IdleTime   : in time;
	                              Level      : in integer range -1 to integer'high; -- -1 = don't check, otherwise interpreted unsigned
	                              Msg        : in string := "";
	                              Prefix     : in string := "###ERROR###: ");

	-- Check when a signal had its last activity (without waiting)
	procedure CheckLastActivity(signal Sig : in std_logic;
	                            IdleTime   : in time;
	                            Level      : in integer range -1 to 1; -- -1 = don't check, 0 = low, 1 = high
	                            Msg        : in string := "";
	                            Prefix     : in string := "###ERROR###: ");

	procedure CheckLastActivityStlv(signal Sig : in std_logic_vector;
	                                IdleTime   : in time;
	                                Level      : in integer range -1 to integer'high; -- -1 = don't check, otherwise interpreted unsigned
	                                Msg        : in string := "";
	                                Prefix     : in string := "###ERROR###: ");

	-- pulse a signal
	procedure PulseSig(signal Sig : out std_logic;
	                   signal Clk : in std_logic);

	-- Clocked wait for a signal
	procedure ClockedWaitFor(Val        : in std_logic;
	                         signal Sig : in std_logic;
	                         signal Clk : in std_logic);
							 
	-- Wait for a number of clock cycles
	procedure WaitClockCycles(	Cycles     : in integer;
								signal Clk : in std_logic);	
				
	-- Wait for a time and quit on rising edge
	procedure ClockedWaitTime(	Duration   	: in time;
								signal Clk 	: in std_logic);	

	-- Strobe generator
	procedure GenerateStrobe(	freq_clock 	 : in real      := 100.0E6; -- in Hz
								freq_str   	 : in real      := 1.0E6; 	-- in Hz
								rst_pol_g  	 : in std_logic := '1'; 	-- reset polarity
								signal rst   : in std_logic; 			-- rst
								signal clk   : in std_logic; 			-- clk
								signal str   : out std_logic); 			-- str
	
	-- check if stdlv is arrived within a defined period of time
	procedure WaitForValueStdlv(signal Sig		: in std_logic_vector; 		-- Signal to check
								ExpVal			: in std_logic_vector; 		-- expected value
								Timeout	  		: in time;					-- time to wait for
								Msg		  		: in string;				-- msg to display
								Prefix     		: in string := "###ERROR###: ");   			-- bool out to stop Tb for ex.
							
	-- check if std is arrived within a defined period of time
	procedure WaitForValueStdl(	signal Sig		: in std_logic; 			-- Signal to check
								ExpVal			: in std_logic; 			-- expected value
								Timeout	  		: in time;					-- time to wait for
								Msg		  		: in string;				-- msg to display
								Prefix     		: in string := "###ERROR###: ");   			-- bool out to stop Tb for ex.						

end psi_tb_activity_pkg;

------------------------------------------------------------------------------
-- Package Body
------------------------------------------------------------------------------
package body psi_tb_activity_pkg is

	-- *** CheckNoActivity ***
	procedure CheckNoActivity(signal Sig : in std_logic;
	                          IdleTime   : in time;
	                          Level      : in integer range -1 to 1; -- -1 = don't check, 0 = low, 1 = high
	                          Msg        : in string := "";
	                          Prefix     : in string := "###ERROR###: ") is
	begin
		wait for IdleTime;
		assert Sig'last_event >= IdleTime
		report Prefix & Msg & "[Unexpected Activity]"
		severity error;
		if Level /= -1 then
			StdlCompare(Level, Sig, "CheckNoActivity: " & Msg, Prefix);
		end if;
	end procedure;

	-- *** CheckNoActivityStlv ***
	procedure CheckNoActivityStlv(signal Sig : in std_logic_vector;
	                              IdleTime   : in time;
	                              Level      : in integer range -1 to integer'high; -- -1 = don't check, otherwise interpreted unsigned
	                              Msg        : in string := "";
	                              Prefix     : in string := "###ERROR###: ") is
	begin
		wait for IdleTime;
		assert Sig'last_event >= IdleTime
		report Prefix & Msg & "[Unexpected Activity]"
		severity error;
		if Level /= -1 then
			StdlvCompareInt(Level, Sig, "CheckNoActivityStlv: " & Msg, false, 0, Prefix);
		end if;
	end procedure;

	-- *** CheckLastActivity ***
	procedure CheckLastActivity(signal Sig : in std_logic;
	                            IdleTime   : in time;
	                            Level      : in integer range -1 to 1; -- -1 = don't check, 0 = low, 1 = high
	                            Msg        : in string := "";
	                            Prefix     : in string := "###ERROR###: ") is
	begin
		assert Sig'last_event >= IdleTime
		report Prefix & Msg & "Unexpected activity, " &
					"[Expeced idle " & time'image(IdleTime) &
					", Actual idle " & time'image(Sig'last_event) & "]"
		severity error;
		if Level /= -1 then
			StdlCompare(Level, Sig, "CheckLastActivity: " & Msg, Prefix);
		end if;
	end procedure;

	-- *** CheckLastActivityStlv ***
	procedure CheckLastActivityStlv(signal Sig : in std_logic_vector;
	                                IdleTime   : in time;
	                                Level      : in integer range -1 to integer'high; -- -1 = don't check, otherwise interpreted unsigned
	                                Msg        : in string := "";
	                                Prefix     : in string := "###ERROR###: ") is
	begin
		assert Sig'last_event >= IdleTime
		report Prefix & Msg & "Unexpected activity, " &
					"[Expeced idle " & time'image(IdleTime) &
					", Actual idle " & time'image(Sig'last_event) & "]"
		severity error;
		if Level /= -1 then
			StdlvCompareInt(Level, Sig, "CheckLastActivityStlv: " & Msg, false, 0, Prefix);
		end if;
	end procedure;

	-- *** PulseSig ***
	procedure PulseSig(signal Sig : out std_logic;
	                   signal Clk : in std_logic) is
	begin
		wait until rising_edge(Clk);
		Sig <= '1';
		wait until rising_edge(Clk);
		Sig <= '0';
	end procedure;

	-- *** ClockedWaitFor ***
	procedure ClockedWaitFor(Val        : in std_logic;
	                         signal Sig : in std_logic;
	                         signal Clk : in std_logic) is
	begin
		wait until rising_edge(Clk) and Sig = Val;
	end procedure;
	
	-- *** ClockedWaitFor ***
	procedure WaitClockCycles(	Cycles     : in integer;
								signal Clk : in std_logic) is
	begin
		for i in 0 to Cycles-1 loop
			wait until rising_edge(Clk);
		end loop;
	end procedure;
	
	-- *** ClockedWaitTime ***
	procedure ClockedWaitTime(	Duration   	: in time;
								signal Clk 	: in std_logic) is
	begin
		wait for Duration;
		wait until rising_edge(Clk);
	end procedure;

	-- *** GenerateStrobe ***
	procedure GenerateStrobe(	freq_clock 			: in real      := 100.0E6;
								freq_str   			: in real      := 1.0E6;
								rst_pol_g  			: in std_logic := '1';
								signal rst          : in std_logic;
								signal clk        	: in std_logic;
								signal str          : out std_logic) is

		variable count_v : integer range 0 to (integer(ceil(freq_clock/freq_str))) := 0;
	begin
		while true loop
		wait until rising_edge(clk);
		if rst = rst_pol_g then
			count_v := 0;
			str     <= '0';
		else
			if count_v /= integer(ceil(freq_clock/freq_str)) - 1 then
				str     <= '0';
				count_v := count_v + 1;
			else
				str     <= '1';
				count_v := 0;
			end if;
		end if;
		end loop;
	end procedure;

	-- *** Wait for Standard logic vector to happen ***
	procedure WaitForValueStdlv(signal Sig		: in std_logic_vector;
								ExpVal			: in std_logic_vector;
								Timeout	  		: in time;
								Msg		  		: in string;
								Prefix     		: in string := "###ERROR###: ") is
	begin
		wait until ExpVal = Sig for timeout;
		if ExpVal /= Sig then
			report 	Prefix & Msg & 
				" Target state not reached" &
				" [Expected " & str(ExpVal) & "(0x" & hstr(ExpVal) & ")" &
				", Received " & str(Sig) & "(0x" & hstr(Sig) & ")" & "]"
				severity error;
		end if;
	end procedure;
	
	-- *** Wait for Standard logic to happen ***
	procedure WaitForValueStdl(	signal Sig		: in std_logic;
								ExpVal			: in std_logic;
								Timeout	  		: in time;
								Msg		  		: in string;
								Prefix     		: in string := "###ERROR###: ") is
	begin
		wait until ExpVal = Sig for timeout;
		if ExpVal /= Sig then
			report Prefix & msg & 
				" Target state not reached" &
				" [Expected " & str(ExpVal) & 
				", Received " & str(Sig) & "]" 
				severity error;
		end if;
	end procedure;

end psi_tb_activity_pkg;
