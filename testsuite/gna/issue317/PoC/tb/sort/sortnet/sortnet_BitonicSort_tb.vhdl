-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
--
-- =============================================================================
-- Authors:					Patrick Lehmann
--
-- Module:					TODO
--
-- Description:
-- ------------------------------------
--		TODO
--
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany
--										 Chair of VLSI-Design, Diagnostics and Architecture
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

library OSVVM;
use			OSVVM.RandomPkg.all;

library PoC;
use			PoC.math.all;
use			PoC.utils.all;
use			PoC.vectors.all;
use			PoC.strings.all;
use			PoC.physical.all;
-- simulation only packages
use			PoC.sim_types.all;
use			PoC.simulation.all;
use			PoC.waveform.all;

library Test;


entity sortnet_BitonicSort_tb is
end entity;


architecture tb of sortnet_BitonicSort_tb is
	constant TAG_BITS								: positive	:= 4;

	constant INPUTS									: positive	:= 64;
	constant DATA_COLUMNS						: positive	:= 2;

	constant KEY_BITS								: positive	:= 32;
	constant DATA_BITS							: positive	:= 64;
	constant META_BITS							: positive	:= TAG_BITS;
	constant PIPELINE_STAGE_AFTER		: natural		:= 2;

	constant LOOP_COUNT							: positive	:= 32; -- 1024;

	constant STAGES									: positive	:= triangularNumber(log2ceil(INPUTS));
	constant DELAY									: natural		:= STAGES / PIPELINE_STAGE_AFTER;

	package P_SORTNET_TB is new Test.sortnet_tb
		generic map (
			META_BITS		=> META_BITS,
			DATA_BITS		=> DATA_BITS,
			INPUTS			=> INPUTS
		);
	use P_SORTNET_TB.all;

	constant CLOCK_FREQ					: FREQ																			:= 100 MHz;
	signal Clock								: std_logic																	:= '1';

	signal Generator_Valid			: std_logic																	:= '0';
	signal Generator_IsKey			: std_logic																	:= '0';
	signal Generator_Data				: T_DATA_VECTOR(INPUTS - 1 downto 0)				:= (others => (others => '0'));
	signal Generator_Meta				: std_logic_vector(META_BITS - 1 downto 0)	:= (others => '0');

	signal Sort_Valid						: std_logic;
	signal Sort_IsKey						: std_logic;
	signal Sort_Data						: T_DATA_VECTOR(INPUTS - 1 downto 0);
	signal Sort_Meta						: std_logic_vector(META_BITS - 1 downto 0);

	signal DataInputMatrix			: T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);
	signal DataOutputMatrix			: T_SLM(INPUTS - 1 downto 0, DATA_BITS - 1 downto 0);

	shared variable ScoreBoard	: PT_SCOREBOARD;

begin
	-- initialize global simulation status
	simInitialize;

	simWriteMessage("SETTINGS");
	simWriteMessage("  INPUTS:    " & integer'image(INPUTS));
	simWriteMessage("  KEY_BITS:  " & integer'image(KEY_BITS));
	simWriteMessage("  DATA_BITS: " & integer'image(DATA_BITS));
	simWriteMessage("  REG AFTER: " & integer'image(PIPELINE_STAGE_AFTER));

	simGenerateClock(Clock, CLOCK_FREQ);

	procGenerator : process
		constant simProcessID			: T_SIM_PROCESS_ID		:= simRegisterProcess("Generator");
		variable RandomVar				: RandomPType;					-- protected type from RandomPkg

		variable KeyInput					: std_logic_vector(KEY_BITS - 1 downto 0);
		variable DataInput				: std_logic_vector(DATA_BITS - KEY_BITS - 1 downto 0);
		variable TagInput					: std_logic_vector(TAG_BITS - 1 downto 0);
		variable Generator_Input	: T_DATA_VECTOR(INPUTS - 1 downto 0);

		function GreaterThan(L : std_logic_vector; R : std_logic_vector) return boolean is
			alias LL is L(KEY_BITS - 1 downto 0);
			alias RR is R(KEY_BITS - 1 downto 0);
		begin
			return unsigned(LL) > unsigned(RR);
		end function;

		variable ScoreBoardData	: T_SCOREBOARD_DATA;
	begin
		RandomVar.InitSeed(RandomVar'instance_name);		-- Generate initial seeds

		Generator_Valid		<= '0';
		Generator_IsKey		<= '0';
		Generator_Input		:= (others => (others => '0'));
		Generator_Meta		<= (others => '0');
		wait until rising_edge(Clock);

		Generator_Valid		<= '1';
		for i in 0 to LOOP_COUNT - 1 loop
--			report "Loop: " & integer'image (i) severity note;
		  TagInput							:= RandomVar.RandSlv(TAG_BITS);

			ScoreBoardData.IsKey	:= to_sl(i mod DATA_COLUMNS = 0);
			ScoreBoardData.Meta		:= resize(TagInput, META_BITS);
			Generator_IsKey				<= ScoreBoardData.IsKey;
			Generator_Meta				<= ScoreBoardData.Meta;

			KeyInput							:= RandomVar.RandSlv(KEY_BITS);
			DataInput							:= RandomVar.RandSlv(DATA_BITS - KEY_BITS);
			Generator_Input(0)		:= DataInput & KeyInput;
			ScoreBoardData.Data(0):= Generator_Input(0);

			loop_j: for j in 1 to INPUTS - 1 loop
				KeyInput						:= RandomVar.RandSlv(KEY_BITS);
				DataInput						:= RandomVar.RandSlv(DATA_BITS - KEY_BITS);
				Generator_Input(j)	:= DataInput & KeyInput;

				for k in j downto 1 loop
					if GreaterThan(ScoreBoardData.Data(k - 1), Generator_Input(j)) then
						ScoreBoardData.Data(k)	:= ScoreBoardData.Data(k - 1);
					else
						ScoreBoardData.Data(k)	:= Generator_Input(j);
						next loop_j;
					end if;
				end loop;
				ScoreBoardData.Data(0)	:= Generator_Input(j);
			end loop;

			Generator_Data				<= Generator_Input;
			ScoreBoard.Push(ScoreBoardData);
			wait until rising_edge(Clock);
		end loop;

		Generator_Valid				<= '0';
		wait until rising_edge(Clock);

		-- This process is finished
		simDeactivateProcess(simProcessID);
		wait;		-- forever
	end process;

	DataInputMatrix		<= to_slm(Generator_Data);

	sort : entity PoC.sortnet_BitonicSort
		generic map (
			INPUTS								=> INPUTS,
			KEY_BITS							=> KEY_BITS,
			DATA_BITS							=> DATA_BITS,
			META_BITS							=> META_BITS,
			PIPELINE_STAGE_AFTER	=> PIPELINE_STAGE_AFTER
		)
		port map (
			Clock				=> Clock,
			Reset				=> '0',

			In_Valid		=> Generator_Valid,
			In_IsKey		=> Generator_IsKey,
			In_Data			=> DataInputMatrix,
			In_Meta			=> Generator_Meta,

			Out_Valid		=> Sort_Valid,
			Out_IsKey		=> Sort_IsKey,
			Out_Data		=> DataOutputMatrix,
			Out_Meta		=> Sort_Meta
		);

	Sort_Data	<= to_dv(DataOutputMatrix);

	procChecker : process
		constant simProcessID		: T_SIM_PROCESS_ID		:= simRegisterProcess("Checker");
		variable Check					: boolean;
		variable CurValue				: unsigned(KEY_BITS - 1 downto 0);
		variable LastValue			: unsigned(KEY_BITS - 1 downto 0);

		variable ScoreBoardData	: T_SCOREBOARD_DATA;
	begin
		wait until rising_edge(Sort_Valid);

		for i in 0 to LOOP_COUNT - 1 loop
			wait until falling_edge(Clock);

			Check		:= TRUE;
			ScoreBoardData.IsKey	:= Sort_IsKey;
			ScoreBoardData.Meta		:= Sort_Meta;
			ScoreBoardData.Data		:= Sort_Data;
			ScoreBoard.Check(ScoreBoardData);
		end loop;
		-- simAssertion(Check, "Result is not monotonic." & raw_format_slv_hex(std_logic_vector(LastValue)));

		-- This process is finished
		simDeactivateProcess(simProcessID);
		wait;  -- forever
	end process;
end architecture;
