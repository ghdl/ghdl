------------------------------------------------------------------------------
--  Copyright (c) 2018 by Paul Scherrer Institute, Switzerland
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
	use work.psi_common_math_pkg.all;

------------------------------------------------------------------------------
-- Package Header
------------------------------------------------------------------------------
package psi_common_logic_pkg is

	function ZerosVector(size : in natural) return std_logic_vector;
	
	function OnesVector(size : in natural) return std_logic_vector;
	
	function ShiftLeft(	arg		: in	std_logic_vector;
						bits	: in	integer;
						fill	: in	std_logic := '0') 
						return std_logic_vector;

	function ShiftRight(arg		: in	std_logic_vector;
						bits	: in	integer;
						fill	: in	std_logic := '0') 
						return std_logic_vector;		

	function BinaryToGray(	binary	: in	std_logic_vector)
							return std_logic_vector;
							
	function GrayToBinary(	gray	: in	std_logic_vector)
							return std_logic_vector;
							
	-- Parallel Prefix Computation of the OR function
	-- Input 	--> Output
	-- 0100		--> 0111
	-- 0101		--> 0111
	-- 0011		--> 0011
	-- 0010		--> 0011
	function PpcOr(	inp	: in	std_logic_vector)
					return std_logic_vector;
					
	function IntToStdLogic(	int 	: in	integer)
							return std_logic;
							
	function ReduceOr(	vec : in std_logic_vector)
						return std_logic;
						
	function ReduceAnd(	vec : in std_logic_vector)
						return std_logic;
						
	function To01X(	inp : in std_logic) 
					return std_logic;
	
	function To01X(	inp : in std_logic_vector)
					return std_logic_vector;

end psi_common_logic_pkg;	 

------------------------------------------------------------------------------
-- Package Body
------------------------------------------------------------------------------
package body psi_common_logic_pkg is 
  
	-- *** ZerosVector ***
	function ZerosVector(size : in natural) return std_logic_vector is
		constant c : std_logic_vector(size-1 downto 0) := (others => '0');
	begin
		return c;
	end function;
	
	-- *** OnesVector ***
	function OnesVector(size : in natural) return std_logic_vector is
		constant c : std_logic_vector(size-1 downto 0) := (others => '1');
	begin
		return c;
	end function;
	
	-- *** ShiftLeft ***
	function ShiftLeft(	arg		: in	std_logic_vector;
						bits	: in	integer;
						fill	: in	std_logic := '0')
						return std_logic_vector	is
		constant argDt : std_logic_vector(arg'high downto 0) := arg;
		variable v : std_logic_vector(argDt'range);
	begin
		if bits < 0 then
			return ShiftRight(argDt, -bits, fill);
		else
			v(v'left downto bits) 	:= argDt(argDt'left-bits downto 0);
			v(bits-1 downto 0)		:= (others => fill);
			return v;
		end if;
	end function;
				
	-- *** ShiftRight ***
	function ShiftRight(	arg		: in	std_logic_vector;
							bits	: in	integer;
							fill	: in	std_logic := '0')
							return std_logic_vector	is
		constant argDt : std_logic_vector(arg'high downto 0) := arg;
		variable v : std_logic_vector(argDt'range);
	begin
		if bits < 0 then
			return ShiftLeft(argDt, -bits, fill);
		else
			v(v'left-bits downto 0) 		:= argDt(argDt'left downto bits);
			v(v'left downto v'left-bits+1)	:= (others => fill);
			return v;
		end if;
	end function;	
	
	-- *** BinaryToGray ***
	function BinaryToGray(	binary	: in	std_logic_vector)
							return std_logic_vector is
		variable Gray_v : std_logic_vector(binary'range);
	begin
		Gray_v := binary xor ('0' & binary(binary'left downto 1));
		return Gray_v;
	end function;
			
	-- *** GrayToBinary ***
	function GrayToBinary(	gray	: in	std_logic_vector)
							return std_logic_vector is
		variable Binary_v : std_logic_vector(gray'range);
	begin
		Binary_v(Binary_v'left) := gray(gray'left);
		for b in gray'left-1 downto 0 loop
			Binary_v(b) := gray(b) xor Binary_v(b+1);
		end loop;		
		return Binary_v;
	end function;

	
	-- *** PpcOr ***
	function PpcOr(	inp	: in	std_logic_vector)
					return std_logic_vector	is
		constant Stages_c		: integer	:= log2ceil(inp'length);
		constant Pwr2Width_c	: integer	:= 2**Stages_c;
		type StageOut_t	is array (natural range <>) of std_logic_vector(Pwr2Width_c-1 downto 0);
		variable StageOut_v		: StageOut_t(0 to Stages_c);			
		variable BinCnt_v		: unsigned(Pwr2Width_c-1 downto 0);
	begin
		StageOut_v(0) 							:= (others => '0');
		StageOut_v(0)(inp'length-1 downto 0)	:= inp;
		for stage in 0 to Stages_c-1 loop		
			BinCnt_v := (others => '0');
			for idx in 0 to Pwr2Width_c-1 loop
				if BinCnt_v(stage) = '0' then
					StageOut_v(stage+1)(idx) := StageOut_v(stage)(idx) or StageOut_v(stage)((idx/(2**stage)+1)*2**stage);
				else
					StageOut_v(stage+1)(idx) := StageOut_v(stage)(idx);
				end if;
				BinCnt_v := BinCnt_v+1;
			end loop;
		end loop;
		return StageOut_v(Stages_c)(inp'length-1 downto 0);
	end function;
	
	function IntToStdLogic(	int 	: in	integer)
							return std_logic is
	begin
		if int = 1 then
			return '1';
		elsif int = 0 then
			return '0';
		else
			return 'X';
		end if;
	end function;
	
	function ReduceOr(	vec : in std_logic_vector)
						return std_logic is
		variable tmp : std_logic;
	begin
		tmp := '0';
		for i in 0 to vec'high loop
			tmp := tmp or vec(i);
		end loop;
		return tmp;
	end function;
		
	function ReduceAnd(	vec : in std_logic_vector)
						return std_logic is
		variable tmp : std_logic;
	begin
		tmp := '1';
		for i in 0 to vec'high loop
			tmp := tmp and vec(i);
		end loop;
		return tmp;
	end function;
	
	function To01X(	inp : in std_logic) 
					return std_logic is
	begin
		case inp is
			when '0' | 'L' => return '0';
			when '1' | 'H' => return '1';
			when others => return 'X';
		end case;
	end function;
	
	function To01X(	inp : in std_logic_vector)
					return std_logic_vector is
		variable tmp : std_logic_vector(inp'range);
	begin
		for i in inp'low to inp'high loop
			tmp(i) := to01X(inp(i));
		end loop;
		return tmp;
	end function;
	
end psi_common_logic_pkg;





