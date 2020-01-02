------------------------------------------------------------------------------
--  Copyright (c) 2019 by Paul Scherrer Institute, Switzerland
--  All rights reserved.
--  Authors: Oliver Bruendler
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Libraries
------------------------------------------------------------------------------
library ieee;
	use ieee.std_logic_1164.all;
	use ieee.numeric_std.all;

library work;
	use work.psi_tb_compare_pkg.all;
	use work.psi_tb_activity_pkg.all;
	use work.psi_tb_txt_util.all;
	use work.psi_common_logic_pkg.all;
	use work.psi_common_math_pkg.all;

------------------------------------------------------------------------------
-- Package Header
------------------------------------------------------------------------------
package psi_tb_i2c_pkg is
	-- -----------------------------------------------------------------------
	-- Constants
	-- -----------------------------------------------------------------------
	constant I2c_ACK 	: std_logic := '0';
	constant I2c_NACK 	: std_logic := '1';
	
	type I2c_Transaction_t is (I2c_READ, I2c_WRITE);
	
	-- -----------------------------------------------------------------------
	-- Functions
	-- -----------------------------------------------------------------------
	function I2cGetAddr( Addr 	: in integer;
						 Trans 	: in I2c_Transaction_t) return integer;

	-- -----------------------------------------------------------------------
	-- Initialization
	-- -----------------------------------------------------------------------
	procedure I2cPullup(signal scl : inout std_logic;
						signal sda : inout std_logic);
						
	procedure I2cBusFree(	signal scl : inout std_logic;
							signal sda : inout std_logic);
						
	procedure I2cSetFrequency(	frequencyHz	: in	real);
	
	-- -----------------------------------------------------------------------
	-- Master Side Transactions
	-- -----------------------------------------------------------------------
	procedure I2cMasterSendStart(	signal Scl 	: inout std_logic;
									signal Sda 	: inout std_logic;
									Msg			: in 	string	:= "No Msg";
									Prefix		: in 	string	:= "###ERROR###: ");
							
	procedure I2cMasterSendRepeatedStart(	signal Scl 	: inout std_logic;
											signal Sda 	: inout std_logic;
											Msg			: in 	string	:= "No Msg";
											Prefix		: in 	string	:= "###ERROR###: ");
									
	procedure I2cMasterSendStop(	signal Scl 	: inout std_logic;
									signal Sda 	: inout std_logic;
									Msg			: in 	string	:= "No Msg";
									Prefix		: in 	string	:= "###ERROR###: ");
							
	procedure I2cMasterSendAddr(	Address 	: in	integer;
									IsRead		: in	boolean;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									AddrBits	: in	integer		:= 7;		-- 7 or 10
									ExpectedAck	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack, anything else for "don't check"
									Prefix		: in 	string		:= "###ERROR###: ");	
								
	procedure I2cMasterSendByte(	Data 		: in	integer range -128 to 255;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									ExpectedAck	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack, anything else for "don't check"
									Prefix		: in 	string		:= "###ERROR###: ");	
								
	procedure I2cMasterExpectByte(	ExpData		: in	integer range -128 to 255;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									AckOutput	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack
									Prefix		: in 	string		:= "###ERROR###: ");	
	
	-- -----------------------------------------------------------------------	
	-- Slave Side Transactions
	-- -----------------------------------------------------------------------
	procedure I2cSlaveWaitStart(	signal Scl 	: inout std_logic;
									signal Sda 	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									Timeout		: in	time		:= 1 ms;
									Prefix		: in 	string		:= "###ERROR###: ");		
							
	procedure I2cSlaveWaitRepeatedStart(	signal Scl 	: inout std_logic;
											signal Sda 	: inout std_logic;
											Msg			: in 	string		:= "No Msg";
											Timeout		: in	time		:= 1 ms;
											ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
											Prefix		: in 	string		:= "###ERROR###: ");
									
	procedure I2cSlaveWaitStop(	signal Scl 	: inout std_logic;
								signal Sda 	: inout std_logic;
								Msg			: in 	string		:= "No Msg";
								Timeout		: in	time		:= 1 ms;
								ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
								Prefix		: in 	string		:= "###ERROR###: ");
							
	procedure I2cSlaveExpectAddr(	Address 	: in	integer;
									IsRead		: in	boolean;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									AddrBits	: in	integer		:= 7;		-- 7 or 10
									AckOutput	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack
									Timeout		: in	time		:= 1 ms;
									ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
									Prefix		: in 	string		:= "###ERROR###: ");	
								
	procedure I2cSlaveExpectByte(	ExpData 	: in	integer range -128 to 255;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									AckOutput	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack
									Timeout		: in	time		:= 1 ms;
									ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
									Prefix		: in 	string		:= "###ERROR###: ");	
								
	procedure I2cSlaveSendByte(	Data		: in	integer range -128 to 255;
								signal Scl	: inout std_logic;
								signal Sda	: inout std_logic;
								Msg			: in 	string		:= "No Msg";
								ExpectedAck	: in 	std_logic	:= '0';			-- '0' for ack, '1' for nack, anything else for "don't check"
								Timeout		: in	time		:= 1 ms;
								ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
								Prefix		: in 	string		:= "###ERROR###: ");	

end psi_tb_i2c_pkg;

------------------------------------------------------------------------------
-- Package Body
------------------------------------------------------------------------------
package body psi_tb_i2c_pkg is

	-- -----------------------------------------------------------------------
	-- Local Types
	-- -----------------------------------------------------------------------
	type MsgInfo_r is record
		Prefix	: string;
		Func	: string;
		User	: string;
	end record;

	-- -----------------------------------------------------------------------
	-- Local Variables
	-- -----------------------------------------------------------------------
	shared variable FreqClk_v	: real	:= 100.0e3;

	-- -----------------------------------------------------------------------
	-- Private Procedures
	-- -----------------------------------------------------------------------
	-- *** Message Handling ***
	function GenMessage(	Prefix	: in string;
							Func 	: in string;
							General	: in string;
							User	: in string) 
							return string is
	begin
		return Prefix & "- " & Func & " - " & General & " - " & User;
	end function;
	
	function GenMessageNoPrefix(	Func 	: in string;
									General	: in string;
									User	: in string) 
									return string is	
	begin
		return Func & " - " & General & " - " & User;
	end function;
	
	-- *** Level Check ***
	procedure LevelCheck(	Expected	: in	std_logic;
							signal Sig	: in	std_logic;
							Msg			: in	MsgInfo_r;
							GeneralMsg	: in	string) is
	begin
		-- Do not check for other inputs than 1 or 0
		if (Expected = '0') or (Expected = '1') then
			assert ((Expected = '0') and (Sig = '0')) or ((Expected = '1') and ((Sig = '1') or (Sig = 'H')))
				report GenMessage(Msg.Prefix, Msg.Func, GeneralMsg, Msg.User)
				severity error;
		end if;
	end procedure;
	
	procedure LevelWait(	Expected	: in	std_logic;
							signal Sig	: in	std_logic;
							Timeout		: in	time;
							Msg			: in	MsgInfo_r;
							GeneralMsg	: in	string) is
		variable Correct_v : boolean;
	begin
		if Sig /= Expected then
			if Expected = '0' then
				wait until Sig = '0' for Timeout;
				Correct_v := (Sig = '0');
			else
				wait until (Sig = '1') or (Sig = 'H') for Timeout;
				Correct_v := ((Sig = '1') or (Sig = 'H'));
			end if;
			assert Correct_v 
				report GenMessage(Msg.Prefix, Msg.Func, GeneralMsg, Msg.User)
				severity error;
		end if;
	end procedure;
	
	
	-- *** Time Calculations ***
	impure function ClkPeriod return time is
	begin
		return (1 sec) / FreqClk_v;
	end function;
	
	impure function ClkHalfPeriod return time is
	begin
		return (0.5 sec) / FreqClk_v;
	end function;	
	
	impure function ClkQuartPeriod return time is
	begin
		return (0.25 sec) / FreqClk_v;
	end function;		
	
	-- *** Bit Transfers ***
	procedure SendBitInclClock(	Data		: in	std_logic;
								signal Scl	: inout std_logic;
								signal Sda	: inout std_logic;
								BitInfo		: in	string;
								Msg			: in 	MsgInfo_r) is
	begin
		-- Initial Check
		LevelCheck('0', Scl, Msg, "SCL must be 0 before SendBitInclClock is called [" & BitInfo & "]");
		
		-- Assert Data		
		if Data = '0' then
			Sda <= '0';
		else
			Sda <= 'Z';
		end if;
		wait for ClkQuartPeriod;
		
		-- Send Clk Pulse
		Scl <= 'Z';
		LevelWait('1', Scl, 1 ms, Msg, "SCL held low by other device");
		wait for ClkHalfPeriod;
		CheckLastActivity(Scl, ClkHalfPeriod*0.9, -1, GenMessageNoPrefix(Msg.Func, "SCL high period too short [" & BitInfo & "]", Msg.User), Msg.Prefix);
		LevelCheck(Data, Sda, Msg, "SDA readback does not match SDA transmit value during SCL pulse [" & BitInfo & "]");
		CheckLastActivity(Sda, ClkHalfPeriod, -1, GenMessageNoPrefix(Msg.Func, "SDA not stable during SCL pulse [" & BitInfo & "]", Msg.User), Msg.Prefix);
		Scl <= '0';
		wait for ClkQuartPeriod;	
	end procedure;
	
	procedure CheckBitInclClock(	Data			: in	std_logic;
									signal Scl		: inout std_logic;
									signal Sda		: inout std_logic;
									BitInfo			: in	string;
									Msg				: in 	MsgInfo_r) is	
	begin
		-- Initial Check
		LevelCheck('0', Scl, Msg, "SCL must be 0 before CheckBitInclClock is called");
		
		-- Wait for assertion
		wait for ClkQuartPeriod;
		
		-- Send Clk Pulse
		Scl <= 'Z';
		LevelWait('1', Scl, 1 ms, Msg, "SCL held low by other device");
		wait for ClkHalfPeriod;
		CheckLastActivity(Scl, ClkHalfPeriod*0.9, -1, GenMessageNoPrefix(Msg.Func, "SCL high period too short [" & BitInfo & "]", Msg.User), Msg.Prefix);
		LevelCheck(Data, Sda, Msg, "Received wrong data [" & BitInfo & "]");
		CheckLastActivity(Sda, ClkHalfPeriod, -1, GenMessageNoPrefix(Msg.Func, "SDA not stable during SCL pulse [" & BitInfo & "]", Msg.User), Msg.Prefix);
		Scl <= '0';
		wait for ClkQuartPeriod;	
	end procedure;	
	
	procedure SendBitExclClock(		Data			: in	std_logic;
									signal Scl		: inout std_logic;
									signal Sda		: inout std_logic;
									Timeout			: in	time;
									BitInfo			: in	string;
									Msg				: in 	MsgInfo_r;
									ClockStretch	: in	time) is	
		variable Stretched_v : boolean := false;
	begin
		-- Initial Check
		LevelCheck('0', Scl, Msg, "SCL must be 0 before SendBitExclClock is called");
		
		-- Clock stretching
		if ClockStretch > 0 ns then
			Scl <= '0';
			wait for ClockStretch;
			Stretched_v := true;
		end if;
		
		-- Assert Data		
		if Data = '0' then
			Sda <= '0';
		else
			Sda <= 'Z';
		end if;	
		if Stretched_v then
			wait for ClkQuartPeriod;
			Scl <= 'Z';
		end if;
		
		-- Wait clock rising edge
		LevelWait('1', Scl, Timeout, Msg, "SCL did not go high");
		
		-- wait clock falling edge
		LevelWait('0', Scl, Timeout, Msg, "SCL did not go low");
		LevelCheck(Data, Sda, Msg, "Received wrong data [" & BitInfo & "]");
		CheckLastActivity(Sda, ClkHalfPeriod, -1, GenMessageNoPrefix(Msg.Func, "SDA not stable during SCL pulse [" & BitInfo & "]", Msg.User), Msg.Prefix);
		
		-- wait until center of low
		wait for ClkQuartPeriod;
	end procedure;		
	
	procedure CheckBitExclClock(	Data			: in	std_logic;
									signal Scl		: inout std_logic;
									signal Sda		: inout std_logic;
									Timeout			: in	time;
									BitInfo			: in	string;
									Msg				: in 	MsgInfo_r;
									ClockStretch	: in	time) is	
	begin
		-- Initial Check
		LevelCheck('0', Scl, Msg, "SCL must be 0 before CheckBitExclClock is called");
		
		-- Wait clock rising edge
		if ClockStretch > 0 ns then
			Scl <= '0';
			wait for ClockStretch;
			Scl <= 'Z';
		end if;		
		LevelWait('1', Scl, Timeout, Msg, "SCL did not go high");
		
		-- wait clock falling edge
		LevelWait('0', Scl, Timeout, Msg, "SCL did not go low");
		LevelCheck(Data, Sda, Msg, "Received wrong data [" & BitInfo & "]");
		CheckLastActivity(Sda, ClkHalfPeriod, -1, GenMessageNoPrefix(Msg.Func, "SDA not stable during SCL pulse [" & BitInfo & "]", Msg.User), Msg.Prefix);
		
		-- wait until center of low
		wait for ClkQuartPeriod;
	end procedure;	
	
	
	-- *** Byte Transfers ***	
	procedure SendByteInclClock(	Data 		: in	std_logic_vector(7 downto 0);
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	MsgInfo_r) is
	begin
		-- Do bits
		for i in 7 downto 0 loop
			SendBitInclClock(Data(i), Scl, Sda, to_string(i), Msg);		
		end loop;
	end procedure;


	
	procedure ExpectByteExclClock(	ExpData 	: in	std_logic_vector(7 downto 0);
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	MsgInfo_r;
									Timeout		: in	time;
									ClkStretch	: in	time) is
	begin
		-- Do bits
		for i in 7 downto 0 loop
			CheckBitExclClock(ExpData(i), Scl, Sda, Timeout, to_string(i), Msg, ClkStretch);	
		end loop;
	end procedure;
	
	-- -----------------------------------------------------------------------
	-- Functions
	-- -----------------------------------------------------------------------
	function I2cGetAddr( Addr 	: in integer;
						 Trans 	: in I2c_Transaction_t) return integer is
	begin
		return Addr*2+choose(Trans=I2c_READ, 1, 0);
	end function;
	
	-- -----------------------------------------------------------------------
	-- Master Side Transactions
	-- -----------------------------------------------------------------------
	procedure I2cPullup(signal Scl : inout std_logic;
						signal Sda : inout std_logic) is
	begin
		Scl <= 'H';
		Sda <= 'H';
	end procedure;
	
	procedure I2cBusFree(	signal Scl : inout std_logic;
							signal Sda : inout std_logic) is
	begin
		Scl <= 'Z';
		Sda <= 'Z';
	end procedure;	
	
	procedure I2cSetFrequency(	FrequencyHz	: in	real) is
	begin
		FreqClk_v := FrequencyHz;
	end procedure;
	
	procedure I2cMasterSendStart(	signal Scl 	: inout std_logic;
									signal Sda 	: inout std_logic;
									Msg			: in 	string	:= "No Msg";
									Prefix		: in 	string	:= "###ERROR###: ") is
		constant MsgInfo : MsgInfo_r := (Prefix, "I2cMasterSendStart", Msg);
	begin
		-- Initial check
		LevelCheck('1', Scl, MsgInfo, "SCL must be 1 before procedure is called");
		LevelCheck('1', Sda, MsgInfo, "SDA must be 1 before procedure is called");
		
		-- Do start condition
		wait for ClkQuartPeriod;
		Sda <= '0';
		LevelCheck('1', Scl, MsgInfo, "SCL must be 1 during SDA falling edge");
		wait for ClkQuartPeriod;
		
		-- Go to center of clk low period
		Scl <= '0';
		wait for ClkQuartPeriod;
	end procedure;
	
	procedure I2cMasterSendRepeatedStart(	signal Scl 	: inout std_logic;
											signal Sda 	: inout std_logic;
											Msg			: in 	string	:= "No Msg";
											Prefix		: in 	string	:= "###ERROR###: ") is
		constant MsgInfo : MsgInfo_r := (Prefix, "I2cMasterSendRepeatedStart", Msg);
	begin
		-- Initial check
		if to01X(Scl) = '1' then
			LevelCheck('1', Sda, MsgInfo, "SDA must be 1 before procedure is called if SCL = 1");
		end if;
		
		-- Do repeated start
		if Scl = '0' then
			Sda <= 'Z';
			wait for ClkQuartPeriod;
			LevelCheck('1', Sda, MsgInfo, "SDA held low by other device");
			Scl <= 'Z';
			wait for ClkQuartPeriod;
			LevelCheck('1', Scl, MsgInfo, "SCL held low by other device");
		end if;
		wait for ClkQuartPeriod;
		Sda <= '0';
		LevelCheck('1', Scl, MsgInfo, "SCL must be 1 during SDA falling edge");
		wait for ClkQuartPeriod;
		
		-- Go to center of clk low period
		Scl <= '0';
		wait for ClkQuartPeriod;
	end procedure;
									
	procedure I2cMasterSendStop(	signal Scl 	: inout std_logic;
									signal Sda 	: inout std_logic;
									Msg			: in 	string	:= "No Msg";
									Prefix		: in 	string	:= "###ERROR###: ") is
		constant MsgInfo : MsgInfo_r := (Prefix, "I2cMasterSendStop", Msg);
	begin
		-- Initial check
		if to01X(Scl) = '1' then
			LevelCheck('0', Sda, MsgInfo, "SDA must be 0 before procedure is called if SCL = 1");
		end if;
		
		-- Do stop
		if Scl = '0' then
			Sda <= '0';
			wait for ClkQuartPeriod;
			Scl <= 'Z';
			wait for ClkQuartPeriod;
			LevelCheck('1', Scl, MsgInfo, "SCL held low by other device");
		else
			wait for ClkQuartPeriod;		
		end if;
		Sda <= 'Z';
		LevelCheck('1', Scl, MsgInfo, "SCL must be 1 during SDA rising edge");
		
		-- Go to center of clk high period
		wait for ClkQuartPeriod;
		
	end procedure;
							
	procedure I2cMasterSendAddr(	Address 	: in	integer;
									IsRead		: in	boolean;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									AddrBits	: in	integer		:= 7;		-- 7 or 10
									ExpectedAck	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack, anything else for "don't check"
									Prefix		: in 	string		:= "###ERROR###: ") is	
		constant AddrSlv_c 	: std_logic_vector(9 downto 0) := std_logic_vector(to_unsigned(Address, 10));
		constant Rw_c 		: std_logic		:= choose(IsRead, '1', '0');
	begin
		-- 7 Bit addressing
		if AddrBits = 7 then
			SendByteInclClock(AddrSlv_c(6 downto 0) & Rw_c, Scl, Sda, (Prefix, "I2cMasterSendAddr 7b", Msg));
			Sda <= 'Z';
			CheckBitInclClock(ExpectedAck, Scl, Sda, "ACK", (Prefix, "I2cMasterSendAddr 7b", Msg));
		-- 10 Bit addressing
		elsif AddrBits = 10 then
			SendByteInclClock("11110" & AddrSlv_c(9 downto 8) & Rw_c, Scl, Sda, (Prefix, "I2cMasterSendAddr 10b 9:8", Msg));
			Sda <= 'Z';
			CheckBitInclClock(ExpectedAck, Scl, Sda, "ACK", (Prefix, "I2cMasterSendAddr 10b 9:8", Msg));
			SendByteInclClock(AddrSlv_c(7 downto 0), Scl, Sda, (Prefix, "I2cMasterSendAddr 10b 7:0", Msg));
			Sda <= 'Z';
			CheckBitInclClock(ExpectedAck, Scl, Sda, "ACK", (Prefix, "I2cMasterSendAddr 10b 7:0", Msg));
		else
			report Prefix & "I2cMasterSendAddr - Illegal addrBits (must be 7 or 10) - " & Msg severity error;
		end if;
	end procedure;
								
	procedure I2cMasterSendByte(	Data 		: in	integer range -128 to 255;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									ExpectedAck	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack, anything else for "don't check"
									Prefix		: in 	string		:= "###ERROR###: ") is	
		variable DataSlv_v : std_logic_vector(7 downto 0);
	begin
		-- Do data
		if Data < 0 then
			DataSlv_v := std_logic_vector(to_signed(Data, 8));
		else
			DataSlv_v := std_logic_vector(to_unsigned(Data, 8));
		end if;
		SendByteInclClock(DataSlv_v, Scl, Sda, (Prefix, "I2cMasterSendByte", Msg));
		Sda <= 'Z';
		CheckBitInclClock(ExpectedAck, Scl, Sda, "ACK", (Prefix, "I2cMasterSendByte", Msg));			
	end procedure;

								
	procedure I2cMasterExpectByte(	ExpData		: in	integer range -128 to 255;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									AckOutput	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack	
									Prefix		: in 	string		:= "###ERROR###: ") is
		variable Data_v 	: std_logic_vector(7 downto 0);
	begin
		-- Convert data
		if ExpData < 0 then
			Data_v := std_logic_vector(to_signed(ExpData, 8));
		else
			Data_v := std_logic_vector(to_unsigned(ExpData, 8));
		end if;
	
		-- do bits
		Sda <= 'Z';
		for i in 7 downto 0 loop
			CheckBitInclClock(Data_v(i), Scl, Sda, to_string(i), (Prefix, "I2cMasterExpectByte", Msg));
		end loop;
		SendBitInclClock(AckOutput, Scl, Sda, "ACK", (Prefix, "I2cMasterExpectByte", Msg));
	end procedure;
	
	-- -----------------------------------------------------------------------	
	-- Slave Side Transactions
	-- -----------------------------------------------------------------------
	procedure I2cSlaveWaitStart(	signal Scl 	: inout std_logic;
									signal Sda 	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									Timeout		: in	time		:= 1 ms;
									Prefix		: in 	string		:= "###ERROR###: ") is
		constant MsgInfo : MsgInfo_r := (Prefix, "I2cSlaveWaitStart", Msg);
	begin
		-- Initial check
		LevelCheck('1', Scl, MsgInfo, "SCL must be 1 before procedure is called");
		LevelCheck('1', Sda, MsgInfo, "SDA must be 1 before procedure is called");
		
		-- Do start checking
		LevelWait('0', Sda, Timeout, MsgInfo, "SDA did not go low");
		LevelCheck('1', Scl, MsgInfo, "SCL must be 1 during SDA falling edge");
		LevelWait('0', Scl, Timeout, MsgInfo, "SCL did not go low");
		LevelCheck('0', Sda, MsgInfo, "SDA must be 0 during SCL falling edge");		
		
		-- Wait for center of SCL low
		wait for ClkQuartPeriod;
	end procedure;
							
	procedure I2cSlaveWaitRepeatedStart(	signal Scl 	: inout std_logic;
											signal Sda 	: inout std_logic;
											Msg			: in 	string		:= "No Msg";
											Timeout		: in	time		:= 1 ms;
											ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
											Prefix		: in 	string	:= "###ERROR###: ") is
		constant MsgInfo : MsgInfo_r := (Prefix, "I2cSlaveWaitRepeatedStart", Msg);
	begin
		-- Initial Check
		if to01X(Scl) = '1' then
			LevelCheck('1', Sda, MsgInfo, "SDA must be 1 before procedure is called if SCL = 1");
		end if;
		
		-- Do Check
		if to01X(Scl) = '0' then
			-- Clock stretching
			if ClkStretch > 0 ns then
				Scl <= '0';
				wait for ClkStretch;
				Scl <= 'Z';
			end if;	
			LevelWait('1', Scl, Timeout, MsgInfo, "SCL did not go high");
			LevelCheck('1', Sda, MsgInfo, "SDA must be 1 before SCL goes high");
		end if;
		LevelWait('0', Sda, Timeout, MsgInfo, "SDA did not go low");
		LevelCheck('1', Scl, MsgInfo, "SCL must be 1 during SDA falling edge");
		LevelWait('0', Scl, Timeout, MsgInfo, "SCL did not go low");
		LevelCheck('0', Sda, MsgInfo, "SDA must be 0 during SCL falling edge");		
		
		-- Wait for center of SCL low
		wait for ClkQuartPeriod;
	end procedure;
									
	procedure I2cSlaveWaitStop(	signal Scl 	: inout std_logic;
								signal Sda 	: inout std_logic;
								Msg			: in 	string		:= "No Msg";
								Timeout		: in	time		:= 1 ms;
								ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
								Prefix		: in 	string	:= "###ERROR###: ") is
		constant MsgInfo : MsgInfo_r := (Prefix, "I2cSlaveWaitStop", Msg);
	begin
		-- Initial check
		if to01X(Scl) = '1' then
			LevelCheck('0', Sda, MsgInfo, "SDA must be 0 before procedure is called if SCL = 1");
		end if;
		
		-- Do Check
		if Scl = '0' then
			-- Clock stretching
			if ClkStretch > 0 ns then
				Scl <= '0';
				wait for ClkStretch;
				Scl <= 'Z';
			end if;	
			LevelWait('1', Scl, Timeout, MsgInfo, "SCL did not go high");
			LevelCheck('0', Sda, MsgInfo, "SDA must be 0 before SCL goes high");
		end if;
		LevelWait('1', Sda, Timeout, MsgInfo, "SDA did not go high");
		LevelCheck('1', Scl, MsgInfo, "SCL must be 1 during SDA rising edge");
		
		-- Wait for center of SCL low
		wait for ClkQuartPeriod;
	end procedure;
							
	procedure I2cSlaveExpectAddr(	Address 	: in	integer;
									IsRead		: in	boolean;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									AddrBits	: in	integer		:= 7;		-- 7 or 10
									AckOutput	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack
									Timeout		: in	time		:= 1 ms;
									ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
									Prefix		: in 	string		:= "###ERROR###: ") is
		constant AddrSlv_c 	: std_logic_vector(9 downto 0) := std_logic_vector(to_unsigned(Address, 10));
		constant Rw_c 		: std_logic		:= choose(IsRead, '1', '0');
	begin
		-- 7 Bit addressing
		if AddrBits = 7 then
			ExpectByteExclClock(AddrSlv_c(6 downto 0) & Rw_c, Scl, Sda, (Prefix, "I2cSlaveExpectAddr 7b", Msg), Timeout, ClkStretch);
			SendBitExclClock(AckOutput, Scl, Sda, Timeout, "ACK", (Prefix, "I2cSlaveExpectAddr 7b ack", Msg), ClkStretch);
			I2cBusFree(Scl, Sda);
		-- 10 Bit addressing
		elsif AddrBits = 10 then
			ExpectByteExclClock("11110" & AddrSlv_c(9 downto 8) & Rw_c, Scl, Sda, (Prefix, "I2cSlaveExpectAddr 10b 9:8" , Msg), Timeout, ClkStretch);
			SendBitExclClock(AckOutput, Scl, Sda, Timeout, "ACK", (Prefix, "I2cSlaveExpectAddr 10b 9:8 ack", Msg), ClkStretch);
			I2cBusFree(Scl, Sda);
			ExpectByteExclClock(AddrSlv_c(7 downto 0), Scl, Sda, (Prefix, "I2cSlaveExpectAddr 10b 7:0", Msg), Timeout, ClkStretch);
			SendBitExclClock(AckOutput, Scl, Sda, Timeout, "ACK", (Prefix, "I2cSlaveExpectAddr 10b 7:0 ack", Msg), ClkStretch);		
			I2cBusFree(Scl, Sda);
		else
			report Prefix & "I2cSlaveExpectAddr - Illegal addrBits (must be 7 or 10) - " & Msg severity error;
		end if;
	end procedure;
								
	procedure I2cSlaveExpectByte(	ExpData 	: in	integer range -128 to 255;
									signal Scl	: inout std_logic;
									signal Sda	: inout std_logic;
									Msg			: in 	string		:= "No Msg";
									AckOutput	: in 	std_logic	:= '0';		-- '0' for ack, '1' for nack
									Timeout		: in	time		:= 1 ms;
									ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
									Prefix		: in 	string		:= "###ERROR###: ") is
		variable Data_v : std_logic_vector(7 downto 0);
	begin
		if ExpData < 0 then
			Data_v := std_logic_vector(to_signed(ExpData, 8));
		else
			Data_v := std_logic_vector(to_unsigned(ExpData, 8));
		end if;
		ExpectByteExclClock(Data_v, Scl, Sda, (Prefix, "I2cSlaveExpectByte", Msg), Timeout, ClkStretch);
		SendBitExclClock(AckOutput, Scl, Sda, Timeout, "ACK", (Prefix, "I2cSlaveExpectByte ack", Msg), ClkStretch);
		I2cBusFree(Scl, Sda);
	end procedure;
							
								
	procedure I2cSlaveSendByte(	Data		: in	integer range -128 to 255;
								signal Scl	: inout std_logic;
								signal Sda	: inout std_logic;
								Msg			: in 	string		:= "No Msg";
								ExpectedAck	: in 	std_logic	:= '0';			-- '0' for ack, '1' for nack, anything else for "don't check"
								Timeout		: in	time		:= 1 ms;
								ClkStretch	: in	time		:= 0 ns;	-- hold clock-low for at least this time
								Prefix		: in 	string		:= "###ERROR###: ") is
		variable Data_v : std_logic_vector(7 downto 0);
	begin
		-- Convert Data
		if Data < 0 then
			Data_v := std_logic_vector(to_signed(Data, 8));
		else
			Data_v := std_logic_vector(to_unsigned(Data, 8));
		end if;
		
		-- Send data
		for i in 7 downto 0 loop
			SendBitExclClock(Data_v(i), Scl, Sda, Timeout, to_string(i), (Prefix, "I2cSlaveSendByte", Msg), ClkStretch);
		end loop;
		
		-- Check ack
		I2cBusFree(Scl, Sda);
		CheckBitExclClock(ExpectedAck, Scl, Sda, Timeout, "ACK", (Prefix, "I2cSlaveSendByte", Msg), ClkStretch);
				
	end procedure;

	
end psi_tb_i2c_pkg;

