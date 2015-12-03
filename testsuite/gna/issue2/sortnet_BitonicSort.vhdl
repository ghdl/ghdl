-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Authors:					Patrick Lehmann
-- Bug-Reproducer:	Sorting Network: Bitonic-Sort
-- Original Source:	See https://github.com/VLSI-EDA/PoC
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
use			IEEE.NUMERIC_STD.all;

package packages is
	type		T_SLM								is array(NATURAL range <>, NATURAL range <>) of STD_LOGIC;
	
	function to_sl(Value : BOOLEAN) return STD_LOGIC;
	function mux(sel : STD_LOGIC; slv0 : STD_LOGIC_VECTOR; slv1 : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
	function slm_slice_rows(slm : T_SLM; High : NATURAL; Low : NATURAL) return T_SLM;
	function slm_merge_rows(slm1 : T_SLM; slm2 : T_SLM) return T_SLM;
end package;

package body packages is
	function to_sl(Value : BOOLEAN) return STD_LOGIC is
	begin
		if (Value = TRUE) then
			return '1';
		else
			return '0';
		end if;
	end function;

	function mux(sel : STD_LOGIC; slv0 : STD_LOGIC_VECTOR; slv1 : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
	begin
		return (slv0 and not (slv0'range => sel)) or (slv1 and (slv1'range => sel));
	end function;
	
	function slm_slice_rows(slm : T_SLM; High : NATURAL; Low : NATURAL) return T_SLM is
		variable Result		: T_SLM(High - Low downto 0, slm'length(2) - 1 downto 0)		:= (others => (others => '0'));
	begin
		for i in 0 to High - Low loop
			for j in 0 to slm'length(2) - 1 loop
				Result(i, j)		:= slm(Low + i, slm'low(2) + j);
			end loop;
		end loop;
		return Result;
	end function;
	
	-- Matrix concatenation: slm_merge_*
	function slm_merge_rows(slm1 : T_SLM; slm2 : T_SLM) return T_SLM is
		constant ROWS			: POSITIVE		:= slm1'length(1) + slm2'length(1);
		constant COLUMNS	: POSITIVE		:= slm1'length(2);
		variable slm			: T_SLM(ROWS - 1 downto 0, COLUMNS - 1 downto 0);
	begin
		for i in slm1'range(1) loop
			for j in slm1'low(2) to slm1'high(2) loop
				slm(i, j)		:= slm1(i, j);
			end loop;
		end loop;
		for i in slm2'range(1) loop
			for j in slm2'low(2) to slm2'high(2) loop
				slm(slm1'length(1) + i, j)		:= slm2(i, j);
			end loop;
		end loop;
		return slm;
	end function;
end package body;

library IEEE;
use			IEEE.STD_LOGIC_1164.all;
use			IEEE.NUMERIC_STD.all;

use			work.packages.all;

entity sortnet_BitonicSort_Merge is
	generic (
		INPUTS			: POSITIVE	:= 8;
		KEY_BITS		: POSITIVE	:= 16;
		DATA_BITS		: POSITIVE	:= 16;
		INVERSE			: BOOLEAN		:= FALSE
	);
	port (
		Clock				: in	STD_LOGIC;
		Reset				: in	STD_LOGIC;
		
		DataInputs	: in	T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
		DataOutputs	: out	T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0)
	);
end entity;

architecture rtl of sortnet_BitonicSort_Merge is
	constant HALF_INPUTS	: NATURAL		:= INPUTS / 2;

	subtype T_DATA				is STD_LOGIC_VECTOR(DATA_BITS - 1 downto 0);
	type T_DATA_VECTOR		is array(NATURAL range <>) of T_DATA;

	function to_dv(slm : T_SLM) return T_DATA_VECTOR is
		variable Result	: T_DATA_VECTOR(slm'range(1));
	begin
		for i in slm'high(1) downto slm'low(1) loop
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
	
	signal DataInputVector		: T_DATA_VECTOR(INPUTS - 1 downto 0);
	signal IntermediateVector	: T_DATA_VECTOR(INPUTS - 1 downto 0);
	
	signal DataInputMatrix1		: T_SLM(HALF_INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataInputMatrix2		: T_SLM(HALF_INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataOutputMatrix1	: T_SLM(HALF_INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataOutputMatrix2	: T_SLM(HALF_INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataOutputVector		: T_DATA_VECTOR(INPUTS - 1 downto 0);
	
begin
	genMergers : if (INPUTS > 1) generate
		DataInputVector		<= to_dv(DataInputs);
	
		genSwitches : for i in 0 to HALF_INPUTS - 1 generate
			signal Smaller		: STD_LOGIC;
			signal Switch			: STD_LOGIC;
		begin
			Smaller <= to_sl(DataInputVector(i)(KEY_BITS - 1 downto 0) < DataInputVector(i + HALF_INPUTS)(KEY_BITS - 1 downto 0));
			Switch	<= Smaller xnor to_sl(INVERSE);
			IntermediateVector(i)								<= mux(Switch, DataInputVector(i),								DataInputVector(i + HALF_INPUTS));
			IntermediateVector(i + HALF_INPUTS)	<= mux(Switch, DataInputVector(i + HALF_INPUTS),	DataInputVector(i));
		end generate;
		
		DataInputMatrix1	<= to_slm(IntermediateVector(HALF_INPUTS - 1 downto 0));
		DataInputMatrix2	<= to_slm(IntermediateVector(INPUTS - 1 downto HALF_INPUTS));
		
		merge1 : entity work.sortnet_BitonicSort_Merge
			generic map (
				INPUTS			=> HALF_INPUTS,
				KEY_BITS		=> KEY_BITS,
				DATA_BITS		=> DATA_BITS,
				INVERSE			=> INVERSE
			)
			port map (
				Clock				=> Clock,
				Reset				=> Reset,
				
				DataInputs	=> DataInputMatrix1,
				DataOutputs	=> DataOutputMatrix1
			);
		merge2 : entity work.sortnet_BitonicSort_Merge
			generic map (
				INPUTS			=> INPUTS - HALF_INPUTS,
				KEY_BITS		=> KEY_BITS,
				DATA_BITS		=> DATA_BITS,
				INVERSE			=> INVERSE
			)
			port map (
				Clock				=> Clock,
				Reset				=> Reset,
				
				DataInputs	=> DataInputMatrix2,
				DataOutputs	=> DataOutputMatrix2
			);
		
		DataOutputs		<= slm_merge_rows(DataOutputMatrix1, DataOutputMatrix2);
	end generate;
	genPassThrough : if (INPUTS = 1) generate
		DataOutputs	<= DataInputs;
	end generate;
end architecture;

library IEEE;
use			IEEE.STD_LOGIC_1164.all;
use			IEEE.NUMERIC_STD.all;

use			work.packages.all;

entity sortnet_BitonicSort_Sort is
	generic (
		INPUTS			: POSITIVE	:= 8;
		KEY_BITS		: POSITIVE	:= 16;
		DATA_BITS		: POSITIVE	:= 16;
		INVERSE			: BOOLEAN		:= FALSE
	);
	port (
		Clock				: in	STD_LOGIC;
		Reset				: in	STD_LOGIC;
		
		DataInputs	: in	T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
		DataOutputs	: out	T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0)
	);
end entity;

architecture rtl of sortnet_BitonicSort_Sort is
	constant HALF_INPUTS			: NATURAL		:= INPUTS / 2;

	signal DataInputMatrix1		: T_SLM(HALF_INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataInputMatrix2		: T_SLM(HALF_INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataOutputMatrix1	: T_SLM(HALF_INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataOutputMatrix2	: T_SLM(HALF_INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	
	signal DataInputMatrix3		: T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataOutputMatrix3	: T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	
begin
	genMergers : if (INPUTS > 1) generate
		DataInputMatrix1	<= slm_slice_rows(DataInputs, HALF_INPUTS - 1, 0);
		DataInputMatrix2	<= slm_slice_rows(DataInputs, INPUTS - 1, HALF_INPUTS);
	
		sort1 : entity work.sortnet_BitonicSort_Sort
			generic map (
				INPUTS			=> HALF_INPUTS,
				KEY_BITS		=> KEY_BITS,
				DATA_BITS		=> DATA_BITS,
				INVERSE			=> FALSE
			)
			port map (
				Clock				=> Clock,
				Reset				=> Reset,
				
				DataInputs	=> DataInputMatrix1,
				DataOutputs	=> DataOutputMatrix1
			);
		sort2 : entity work.sortnet_BitonicSort_Sort
			generic map (
				INPUTS			=> INPUTS - HALF_INPUTS,
				KEY_BITS		=> KEY_BITS,
				DATA_BITS		=> DATA_BITS,
				INVERSE			=> TRUE
			)
			port map (
				Clock				=> Clock,
				Reset				=> Reset,
				
				DataInputs	=> DataInputMatrix2,
				DataOutputs	=> DataOutputMatrix2
			);
		
		DataInputMatrix3	<= slm_merge_rows(DataInputMatrix1, DataInputMatrix2);
		
		merge : entity work.sortnet_BitonicSort_Merge
			generic map (
				INPUTS			=> INPUTS,
				KEY_BITS		=> KEY_BITS,
				DATA_BITS		=> DATA_BITS,
				INVERSE			=> INVERSE
			)
			port map (
				Clock				=> Clock,
				Reset				=> Reset,
				
				DataInputs	=> DataInputMatrix3,
				DataOutputs	=> DataOutputMatrix3
			);
		
		DataOutputs		<= DataOutputMatrix3;
	end generate;
	genPassThrough : if (INPUTS = 1) generate
		DataOutputs		<= DataInputs;
	end generate;
end architecture;

library IEEE;
use			IEEE.STD_LOGIC_1164.all;
use			IEEE.NUMERIC_STD.all;

use			work.packages.all;

entity sortnet_BitonicSort is
	generic (
		INPUTS								: POSITIVE	:= 8;
		KEY_BITS							: POSITIVE	:= 16;
		DATA_BITS							: POSITIVE	:= 16;
		ADD_OUTPUT_REGISTERS	: BOOLEAN		:= TRUE
	);
	port (
		Clock									: in	STD_LOGIC;
		Reset									: in	STD_LOGIC;
		
		DataInputs						: in	T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
		DataOutputs						: out	T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0)
	);
end entity;


architecture rtl of sortnet_BitonicSort is
	signal DataInputMatrix1		: T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataOutputMatrix1	: T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	
begin
	DataInputMatrix1		<= DataInputs;
	
	sort : entity work.sortnet_BitonicSort_Sort
		generic map (
			INPUTS			=> INPUTS,
			KEY_BITS		=> KEY_BITS,
			DATA_BITS		=> DATA_BITS,
			INVERSE			=> FALSE
		)
		port map (
			Clock				=> Clock,
			Reset				=> Reset,
			
			DataInputs	=> DataInputMatrix1,
			DataOutputs	=> DataOutputMatrix1
		);
	
	genOutReg : if (ADD_OUTPUT_REGISTERS = TRUE) generate
		DataOutputs	<= DataOutputMatrix1 when rising_edge(Clock);
	end generate;
	genNoOutReg : if (ADD_OUTPUT_REGISTERS = FALSE) generate
		DataOutputs	<= DataOutputMatrix1;
	end generate;
end architecture;
