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

library PoC;
use			PoC.utils.all;
use			PoC.vectors.all;
use			PoC.strings.all;


package sortnet_tb is
	generic (
		META_BITS			: positive;
		DATA_BITS			: positive;
		INPUTS				: positive
	);

	subtype T_DATA				is std_logic_vector(DATA_BITS - 1 downto 0);
	type T_DATA_VECTOR		is array(natural range <>) of T_DATA;

	function to_dv(slm : T_SLM) return T_DATA_VECTOR;
	function to_slm(dv : T_DATA_VECTOR) return T_SLM;

	type T_SCOREBOARD_DATA is record
		IsKey : std_logic;
		Meta  : std_logic_vector(META_BITS - 1 downto 0);
		Data  : T_DATA_VECTOR(INPUTS - 1 downto 0);
	end record;

	function match(expected : T_SCOREBOARD_DATA; actual : T_SCOREBOARD_DATA) return boolean;
	function to_string(dataset : T_SCOREBOARD_DATA) return string;

	package P_SCOREBOARD is new OSVVM.ScoreboardGenericPkg
		generic map (
			ExpectedType        => T_SCOREBOARD_DATA,
			ActualType          => T_SCOREBOARD_DATA,
			Match               => match,
			expected_to_string  => to_string, --[T_SCOREBOARD_DATA return string],
			actual_to_string    => to_string
		);

	alias PT_SCOREBOARD is P_SCOREBOARD.ScoreBoardPType;
end package;


package body sortnet_tb is
	function match(expected : T_SCOREBOARD_DATA; actual : T_SCOREBOARD_DATA) return boolean is
		variable good : boolean;
	begin
		good :=						(expected.IsKey = actual.IsKey);
		good := good and	(expected.Meta = actual.Meta);
		if (expected.IsKey = '1') then
			for i in expected.Data'range loop
				good := good and	(expected.Data(i) = actual.Data(i));
				exit when (good = FALSE);
			end loop;
		end if;
		return good;
	end function;

	function to_string(dataset : T_SCOREBOARD_DATA) return string is
		variable KeyMarker : string(1 to 2);
	begin
		KeyMarker := ite((dataset.IsKey = '1'), "* ", "  ");
		-- for i in 0 to 0 loop --dataset.Key'range loop
			return	"Data: " & to_string(dataset.Data(0), 'h') & KeyMarker &
						"  Meta: " & to_string(dataset.Meta, 'h');
		-- end loop;
	end function;

	function to_dv(slm : T_SLM) return T_DATA_VECTOR is
		variable Result	: T_DATA_VECTOR(slm'range(1));
	begin
		for i in slm'high(1) downto slm'low(1) loop
			for j in T_DATA'range loop
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
end package body;
