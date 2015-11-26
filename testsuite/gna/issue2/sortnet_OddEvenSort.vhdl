-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Authors:					Patrick Lehmann
--
-- Module:					Sorting Network: Odd-Even-Sort (Transposition)
--
-- Description:
-- ------------------------------------
--	TODO
--
-- License:
-- =============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany
--										 Chair for VLSI-Design, Diagnostics and Architecture
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--		http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- =============================================================================

library IEEE;
use			IEEE.STD_LOGIC_1164.all;

package vectors is
	type		T_SLM								is array(NATURAL range <>, NATURAL range <>) of STD_LOGIC;
end package;


library IEEE;
use			IEEE.STD_LOGIC_1164.all;
use			IEEE.NUMERIC_STD.all;

--library PoC;
-- use			PoC.utils.all;
--use			PoC.vectors.all;
--use			PoC.components.all;
use			work.vectors.all;


entity sortnet_OddEvenSort is
	generic (
		INPUTS								: POSITIVE	:= 8;
		KEY_BITS							: POSITIVE	:= 16;
		DATA_BITS							: NATURAL		:= 16;
		PIPELINE_STAGE_AFTER	: NATURAL		:= 2;
		ADD_OUTPUT_REGISTERS	: BOOLEAN		:= TRUE;
		INVERSE								: BOOLEAN		:= FALSE
	);
	port (
		Clock				: in	STD_LOGIC;
		Reset				: in	STD_LOGIC;
		
		DataInputs	: in	T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
		DataOutputs	: out	T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0)
	);
end entity;


architecture rtl of sortnet_OddEvenSort is
	constant C_VERBOSE	: BOOLEAN	:= FALSE;

	constant STAGES			: POSITIVE		:= INPUTS;

	subtype T_DATA			is STD_LOGIC_VECTOR(DATA_BITS - 1 downto 0);
	type T_DATA_VECTOR	is array(NATURAL range <>) of T_DATA;
	type T_DATA_MATRIX	is array(NATURAL range <>, NATURAL range <>) of T_DATA;

	function to_dv(slm : T_SLM) return T_DATA_VECTOR is
		variable Result	: T_DATA_VECTOR(slm'range(1));
	begin
		for i in slm'range(1) loop
			for j in slm'high(2) downto slm'low(2) loop
				Result(i)(j)	:= slm(i, j);
			end loop;
		end loop;
		return Result;
	end function;
	
	function to_slm(dv : T_DATA_VECTOR) return T_SLM is
		variable Result	: T_SLM(dv'range, T_DATA'range);
	begin
		for i in dv'range loop
			for j in T_DATA'range loop
				Result(i, j)	:= dv(i)(j);
			end loop;
		end loop;
		return Result;
	end function;
	
	function mux(sel : STD_LOGIC; slv0 : STD_LOGIC_VECTOR; slv1 : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
	begin
		return (slv0 and not (slv0'range => sel)) or (slv1 and (slv1'range => sel));
	end function;
	
	signal DataInputVector	: T_DATA_VECTOR(INPUTS - 1 downto 0);
	signal DataInputMatrix	: T_DATA_MATRIX(STAGES - 1 downto 0, INPUTS - 1 downto 0);
	signal DataOutputMatrix	: T_DATA_MATRIX(STAGES - 1 downto 0, INPUTS - 1 downto 0);
	signal DataOutputVector	: T_DATA_VECTOR(INPUTS - 1 downto 0);
	
begin
	DataInputVector	<= to_dv(DataInputs);

	genInputs : for i in 0 to INPUTS - 1 generate
		DataInputMatrix(0, i)	<= DataInputVector(i);
	end generate;
	
	genConStage : for stage in 0 to STAGES - 2 generate
		constant INSERT_REGISTER : BOOLEAN	:= ((PIPELINE_STAGE_AFTER > 0) and (stage mod PIPELINE_STAGE_AFTER = 0));
	begin
		genCon : for i in 0 to INPUTS - 1 generate
			genPipeStage : if (INSERT_REGISTER = TRUE) generate
				DataInputMatrix(stage + 1, i)	<= DataOutputMatrix(stage, i)	when rising_edge(Clock);
			end generate;
			genNoPipeStage : if (INSERT_REGISTER = FALSE) generate
				DataInputMatrix(stage + 1, i)	<= DataOutputMatrix(stage, i);
			end generate;
		end generate;
	end generate;
	
	genSwitchStage : for stage in 0 to STAGES - 1 generate
	begin
		genEven : if (stage mod 2 = 0) generate
			genEvenSwitch : for i in 0 to (INPUTS / 2) - 1 generate
				signal DataIn0		: STD_LOGIC_VECTOR(DATA_BITS - 1 downto 0);
				signal DataIn1		: STD_LOGIC_VECTOR(DATA_BITS - 1 downto 0);
			
				signal Greater		: STD_LOGIC;
				signal Switch			: STD_LOGIC;
			begin
				DataIn0		<= DataInputMatrix(stage, 2 * i);
				DataIn1		<= DataInputMatrix(stage, 2 * i + 1);
			
				Greater		<= '1' when (unsigned(DataIn0(KEY_BITS - 1 downto 0)) > unsigned(DataIn1(KEY_BITS - 1 downto 0))) else '0';
				Switch		<= Greater;
				
				DataOutputMatrix(stage, 2 * i)			<= mux(Switch, DataIn0, DataIn1);
				DataOutputMatrix(stage, 2 * i + 1)	<= mux(Switch, DataIn1, DataIn0);
			end generate;
		end generate;
		genOdd : if (stage mod 2 = 1) generate
			DataOutputMatrix(stage, 0)					<= DataInputMatrix(stage, 0);
			DataOutputMatrix(stage, INPUTS - 1)	<= DataInputMatrix(stage, INPUTS - 1);
			
			genOddSwitch : for i in 0 to ((INPUTS - 1) / 2) - 1 generate
				signal DataIn0		: STD_LOGIC_VECTOR(DATA_BITS - 1 downto 0);
				signal DataIn1		: STD_LOGIC_VECTOR(DATA_BITS - 1 downto 0);
			
				signal Greater		: STD_LOGIC;
				signal Switch			: STD_LOGIC;
			begin
				DataIn0		<= DataInputMatrix(stage, 2 * i + 1);
				DataIn1		<= DataInputMatrix(stage, 2 * i + 2);
			
				Greater		<= '1' when (unsigned(DataIn0(KEY_BITS - 1 downto 0)) > unsigned(DataIn1(KEY_BITS - 1 downto 0))) else '0';
				Switch		<= Greater;
				
				DataOutputMatrix(stage, 2 * i + 1)	<= mux(Switch, DataIn0, DataIn1);
				DataOutputMatrix(stage, 2 * i + 2)	<= mux(Switch, DataIn1, DataIn0);
			end generate;
		end generate;
	end generate;

	genOutputs : for i in 0 to INPUTS - 1 generate
		DataOutputVector(i)		<= DataOutputMatrix(STAGES - 1, i);
	end generate;

	genOutReg : if (ADD_OUTPUT_REGISTERS = TRUE) generate
		DataOutputs	<= to_slm(DataOutputVector)	when rising_edge(Clock);
	end generate;
	genNoOutReg : if (ADD_OUTPUT_REGISTERS = FALSE) generate
		DataOutputs	<= to_slm(DataOutputVector);
	end generate;
end architecture;
