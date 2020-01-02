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

library work;
	use work.psi_tb_txt_util.all;

------------------------------------------------------------------------------
-- Package Header
------------------------------------------------------------------------------
package psi_tb_compare_pkg is

	-- returns an index string in the form "[3]"
	function IndexString(	Index 		: integer) return string;
		
	-- std_logic_vector compare to integer
	procedure StdlvCompareInt (	Expected	: in integer;
								Actual		: in std_logic_vector;
								Msg			: in string;
								IsSigned	: in boolean := true;
								Tolerance	: in integer := 0;
								Prefix		: in string	:= "###ERROR###: ");
	
	-- std_logic_vector compare to std_logic_vector							
	procedure StdlvCompareStdlv (Expected	: in std_logic_vector;
								 Actual		: in std_logic_vector;
								 Msg		: in string;
								 Prefix		: in string	:= "###ERROR###: ");	

	-- std_logic compare std_logic
	procedure StdlCompare(	Expected 	: in integer range 0 to 1;
							Actual		: in std_logic;
							Msg			: in string;
							Prefix		: in string	:= "###ERROR###: ");
	
	-- integer compare to integer					
	procedure IntCompare(	Expected 	: in integer;
							Actual		: in integer;
							Msg			: in string;
							Tolerance	: in integer := 0;
							Prefix		: in string	:= "###ERROR###: ");	
	
	-- real compare to real						
	procedure RealCompare(	Expected 	: in real;
							Actual		: in real;
							Msg			: in string;
							Tolerance	: in real := 0.0;
							Prefix		: in string	:= "###ERROR###: ");	
	
	-- signed compare to signed																
	procedure SignCompare (	Expected	: in signed;
							Actual		: in signed;
							Msg			: in string;
							Tolerance	: in integer := 0;
							Prefix		: in string	:= "###ERROR###: ");	
	
	-- unsigned compare to unsigned						
	procedure UsignCompare (Expected	: in unsigned;
							Actual		: in unsigned;
							Msg			: in string;
							Tolerance	: in integer := 0;
							Prefix		: in string	:= "###ERROR###: ");	
	
	-- signed compare to integer						
	procedure SignCompareInt (	Expected	: in integer;
								Actual		: in signed;
								Msg			: in string;
								Tolerance	: in integer := 0;
								Prefix		: in string	:= "###ERROR###: ");	
	
	-- unsigned compare to integer						
	procedure UsignCompareInt (	Expected	: in integer;
								Actual		: in unsigned;
								Msg			: in string;
								Tolerance	: in integer := 0;
								Prefix		: in string	:= "###ERROR###: ");																			
	
end psi_tb_compare_pkg;

------------------------------------------------------------------------------
-- Package Body
------------------------------------------------------------------------------
package body psi_tb_compare_pkg is

		-- *** IndexString ***
		function IndexString(	Index 		: integer) return string is
		begin
			return "[" & to_string(Index) & "]";
		end function;

		-- *** StdlvCompareInt ***
		procedure StdlvCompareInt (	Expected	: in integer;
									Actual		: in std_logic_vector;
									Msg			: in string;
									IsSigned	: in boolean := true;
									Tolerance	: in integer := 0;
									Prefix		: in string	:= "###ERROR###: ") is
			variable ActualInt_v		: integer;
			variable ExpectedStdlv32_v	: std_logic_vector(31 downto 0);
			variable ActualStdlv32_v	: std_logic_vector(31 downto 0);
		begin
			-- Convert Input
			if IsSigned then
				ActualInt_v 		:= to_integer(signed(Actual));
				ExpectedStdlv32_v	:= std_logic_vector(to_signed(Expected, 32));
				ActualStdlv32_v		:= std_logic_vector(to_signed(ActualInt_v, 32));
			else
				ActualInt_v := to_integer(unsigned(Actual));
				ExpectedStdlv32_v	:= std_logic_vector(to_unsigned(Expected, 32));
				ActualStdlv32_v		:= std_logic_vector(to_unsigned(ActualInt_v, 32));				
			end if;
			-- Assertion
			assert (ActualInt_v >= Expected-Tolerance) and (ActualInt_v <= Expected+Tolerance)
				report 	Prefix & Msg & 
						" [Expected " & integer'image(Expected) & "(0x" & hstr(ExpectedStdlv32_v) & ")" &
						", Received " & integer'image(ActualInt_v) & "(0x" & hstr(ActualStdlv32_v) & ")" &
						", Tolerance " & integer'image(Tolerance) & "]"
				severity error;
		end procedure;
		
		-- *** StdlvCompareStdlv ***
		procedure StdlvCompareStdlv (	Expected	: in std_logic_vector;
										Actual		: in std_logic_vector;
										Msg			: in string;
										Prefix		: in string	:= "###ERROR###: ") is
			constant Expected_c	: std_logic_vector(Expected'length-1 downto 0) 	:= Expected;
			constant Actual_c	: std_logic_vector(Actual'length-1 downto 0)	:= Actual;
		begin
			-- Assertion
			assert Actual_c = Expected_c
				report 	Prefix & Msg & 
						" [Expected " & str(Expected_c) & "(0x" & hstr(Expected_c) & ")" &
						", Received " & str(Actual_c) & "(0x" & hstr(Actual_c) & ")" & "]"
				severity error;
		end procedure;		
	
	-- *** StdlCompare ***
	procedure StdlCompare(	Expected 	: in integer range 0 to 1;
							Actual		: in std_logic;
							Msg			: in string;
							Prefix		: in string	:= "###ERROR###: ") is
		variable ExStdl_v : std_logic; 
	begin
		if Expected = 0 then
			ExStdl_v := '0';
		else
			ExStdl_v := '1';
		end if;
		assert Actual = ExStdl_v
				report 	Prefix & Msg & 
						" [Expected " & str(ExStdl_v) & 
						", Received " & str(Actual) & "]"
				severity error;
	end procedure;
	
	-- *** IntCompare ***
	procedure IntCompare(	Expected 	: in integer;
							Actual		: in integer;
							Msg			: in string;
							Tolerance	: in integer := 0;
							Prefix		: in string	:= "###ERROR###: ") is 
	begin
		assert (Actual >= Expected-Tolerance) and (Actual <= Expected+Tolerance)
				report 	Prefix & Msg & 
						" [Expected " & to_string(Expected) & 
						", Received " & to_string(Actual) & 
						", Tolerance " & to_string(Tolerance) & "]"
				severity error;
	end procedure;	
	
	-- *** RealCompare ***
	procedure RealCompare(	Expected 	: in real;
							Actual		: in real;
							Msg			: in string;
							Tolerance	: in real := 0.0;
							Prefix		: in string	:= "###ERROR###: ") is 
	begin
		assert (Actual >= Expected-Tolerance) and (Actual <= Expected+Tolerance)
				report 	Prefix & Msg & 
						" [Expected " & to_string(Expected) & 
						", Received " & to_string(Actual) & 
						", Tolerance " & to_string(Tolerance) & "]"
				severity error;
	end procedure;	
	
	-- *** SignCompare ***
	procedure SignCompare(	Expected 	: in signed;
							Actual		: in signed;
							Msg			: in string;
							Tolerance	: in integer := 0;
							Prefix		: in string	:= "###ERROR###: ") is 
	begin
		assert (Actual >= Expected-Tolerance) and (Actual <= Expected+Tolerance)
				report 	Prefix & Msg & 
						" [Expected " & to_string(Expected) & 
						", Received " & to_string(Actual) & 
						", Tolerance " & to_string(Tolerance) & "]"
				severity error;
	end procedure;
	
	-- *** UsignCompare ***
	procedure UsignCompare(	Expected 	: in unsigned;
							Actual		: in unsigned;
							Msg			: in string;
							Tolerance	: in integer := 0;
							Prefix		: in string	:= "###ERROR###: ") is 
	begin
		assert (Actual >= Expected-Tolerance) and (Actual <= Expected+Tolerance)
				report 	Prefix & Msg & 
						" [Expected " & to_string(Expected) & 
						", Received " & to_string(Actual) & 
						", Tolerance " & to_string(Tolerance) & "]"
				severity error;
	end procedure;
	
	-- *** SignCompareInt ***
	procedure SignCompareInt (	Expected	: in integer;
								Actual		: in signed;
								Msg			: in string;
								Tolerance	: in integer := 0;
								Prefix		: in string	:= "###ERROR###: ") is
		begin		
			StdlvCompareInt (	Expected	=> Expected,
								Actual		=> std_logic_vector(Actual),
								Msg			=> Msg,
								IsSigned	=> true,
								Tolerance	=> Tolerance,
								Prefix		=> Prefix);			
	end procedure;	
	
	-- *** UsignCompareInt ***
	procedure UsignCompareInt (	Expected	: in integer;
								Actual		: in unsigned;
								Msg			: in string;
								Tolerance	: in integer := 0;
								Prefix		: in string	:= "###ERROR###: ") is
		begin		
			StdlvCompareInt (	Expected	=> Expected,
								Actual		=> std_logic_vector(Actual),
								Msg			=> Msg,
								IsSigned	=> false,
								Tolerance	=> Tolerance,
								Prefix		=> Prefix);
	end procedure;	
	
end psi_tb_compare_pkg;

