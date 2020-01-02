------------------------------------------------------------------------------
--  Copyright (c) 2018 by Paul Scherrer Institute, Switzerland
--  All rights reserved.
--  Authors: Oliver Bruendler
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Description
------------------------------------------------------------------------------
-- This is a very basic clock crossing that allows passing multple independent
-- single-bit signals from one clock domain to another one.
-- Double stage synchronizers are implemeted for each bit, including then
-- required attributes.
--
------------------------------------------------------------------------------
-- Libraries
------------------------------------------------------------------------------
library ieee ;
	use ieee.std_logic_1164.all;
	use ieee.numeric_std.all;
	
------------------------------------------------------------------------------
-- Entity Declaration
------------------------------------------------------------------------------
entity psi_common_bit_cc is
	generic (
		NumBits_g		: positive		:= 1
	);
	port (
		-- Clock Domain A
		BitsA		: in 	std_logic_vector(NumBits_g-1 downto 0);
		
		-- Clock Domain B
		ClkB		: in	std_logic;
		BitsB		: out	std_logic_vector(NumBits_g-1 downto 0)
	);
end entity;
		
------------------------------------------------------------------------------
-- Architecture Declaration
------------------------------------------------------------------------------
architecture rtl of psi_common_bit_cc is

	signal Reg0	: std_logic_vector(NumBits_g-1 downto 0)	:= (others => '0');
	signal Reg1 : std_logic_vector(NumBits_g-1 downto 0)	:= (others => '0');
	
	attribute syn_srlstyle : string;
    attribute syn_srlstyle of Reg0 	: signal is "registers";
    attribute syn_srlstyle of Reg1 	: signal is "registers";
	
	attribute shreg_extract : string;
    attribute shreg_extract of Reg0 	: signal is "no";
    attribute shreg_extract of Reg1 	: signal is "no";
	
	attribute ASYNC_REG : string;
    attribute ASYNC_REG of Reg0 		: signal is "TRUE";
    attribute ASYNC_REG of Reg1 		: signal is "TRUE";
	
begin
	
	-- Process
	p : process(ClkB)
	begin
		if rising_edge(ClkB) then
			Reg0 <= BitsA;
			Reg1 <= Reg0;
		end if;
	end process;
	BitsB <= Reg1;
end;





