-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
--									Thomas B. Preusser
--
-- Package:					Simulation constants, functions and utilities.
--
-- Description:
-- -------------------------------------
-- .. TODO:: No documentation available.
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
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;
use			IEEE.math_real.all;

library PoC;
use			PoC.utils.all;
-- use			PoC.strings.all;
use			PoC.vectors.all;
-- use			PoC.physical.all;


package sim_types is

	constant C_SIM_VERBOSE					: boolean		:= FALSE;		-- POC_VERBOSE

	-- ===========================================================================
  -- Simulation Task and Status Management
	-- ===========================================================================
	type		T_SIM_BOOLVEC						is array(integer range <>) of boolean;

	subtype T_SIM_TEST_ID						is integer range -1 to 1023;
	subtype T_SIM_TEST_NAME					is string(1 to 256);
	subtype T_SIM_PROCESS_ID				is natural range 0 to 1023;
	subtype T_SIM_PROCESS_NAME			is string(1 to 64);
	subtype T_SIM_PROCESS_INSTNAME	is string(1 to 256);
	type		T_SIM_PROCESS_ID_VECTOR	is array(natural range <>) of T_SIM_PROCESS_ID;

	type T_SIM_TEST_STATUS is (
		SIM_TEST_STATUS_CREATED,
		SIM_TEST_STATUS_ACTIVE,
		SIM_TEST_STATUS_ENDED,
		SIM_TEST_STATUS_ZOMBI
	);

	type T_SIM_PROCESS_STATUS is (
		SIM_PROCESS_STATUS_ACTIVE,
		SIM_PROCESS_STATUS_ENDED
	);

	type T_SIM_TEST is record
		ID									: T_SIM_TEST_ID;
		Name								: T_SIM_TEST_NAME;
		Status							: T_SIM_TEST_STATUS;
		ProcessIDs					: T_SIM_PROCESS_ID_VECTOR(T_SIM_PROCESS_ID);
		ProcessCount				: T_SIM_PROCESS_ID;
		ActiveProcessCount	: T_SIM_PROCESS_ID;
	end record;
	type T_SIM_TEST_VECTOR	is array(integer range <>) of T_SIM_TEST;

	type T_SIM_PROCESS is record
		ID						: T_SIM_PROCESS_ID;
		TestID				: T_SIM_TEST_ID;
		Name					: T_SIM_PROCESS_NAME;
		Status				: T_SIM_PROCESS_STATUS;
		IsLowPriority	: boolean;
	end record;
	type T_SIM_PROCESS_VECTOR is array(natural range <>) of T_SIM_PROCESS;

	constant C_SIM_DEFAULT_TEST_ID		: T_SIM_TEST_ID		:= -1;
	constant C_SIM_DEFAULT_TEST_NAME	: string					:= "Default test";

	-- ===========================================================================
	-- Random Numbers
	-- ===========================================================================
	type T_SIM_RAND_SEED is record
		Seed1	: integer;
		Seed2	: integer;
	end record;

	procedure randInitializeSeed(Seed : inout T_SIM_RAND_SEED);
	procedure randInitializeSeed(Seed : inout T_SIM_RAND_SEED; SeedValue : in T_SIM_RAND_SEED);
	procedure randInitializeSeed(Seed : inout T_SIM_RAND_SEED; SeedVector : in T_INTVEC);
	procedure randInitializeSeed(Seed : inout T_SIM_RAND_SEED; SeedVector : in string);
	function	randInitializeSeed return T_SIM_RAND_SEED;
	function	randInitializeSeed(SeedValue : T_SIM_RAND_SEED) return T_SIM_RAND_SEED;
	function	randInitializeSeed(SeedVector : T_INTVEC) return T_SIM_RAND_SEED;
	function	randInitializeSeed(SeedVector : string) return T_SIM_RAND_SEED;


	-- Uniform distributed random values
	-- ===========================================================================
	procedure randUniformDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL);
	procedure randUniformDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out integer; Minimum : integer; Maximum : integer);
	procedure randUniformDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; Minimum : REAL; Maximum : REAL);

	-- Normal / Gaussian distributed random values
	-- ===========================================================================
	procedure randNormalDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; StandardDeviation : REAL := 1.0; Mean : REAL := 0.0);
	procedure randNormalDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out integer; StandardDeviation : in REAL; Mean : in REAL; Minimum : in integer; Maximum : in integer);
	procedure randNormalDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; StandardDeviation : in REAL; Mean : in REAL; Minimum : in REAL; Maximum : in REAL);

	-- Poisson distributed random values
	-- ===========================================================================
	procedure randPoissonDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; Mean : in REAL);
	procedure randPoissonDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out integer; Mean : in REAL; Minimum : in integer; Maximum : in integer);
	procedure randPoissonDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; Mean : in REAL; Minimum : in REAL; Maximum : in REAL);

	-- ===========================================================================
	-- Clock Generation
	-- ===========================================================================
	-- type T_PERCENT is INTEGER'range units
	type T_PERCENT is range integer'low to INTEGER'high units
		ppb;
		ppm			= 1000 ppb;
		permil	= 1000 ppm;
		percent	= 10 permil;
		one			= 100 percent;
	end units;
	subtype T_WANDER		is T_PERCENT range -1 one to 1 one;
	subtype T_DUTYCYCLE	is T_PERCENT range	0 ppb to 1 one;

	type T_DEGREE is range integer'low to INTEGER'high units
		second;
		minute	= 60 second;
		deg			= 60 minute;
	end units;
	subtype T_PHASE is T_DEGREE range	-360 deg to 360 deg;

	function ite(cond : boolean; value1 : T_DEGREE; value2 : T_DEGREE) return T_DEGREE;
end package;


package body sim_types is
	function ite(cond : boolean; value1 : T_DEGREE; value2 : T_DEGREE) return T_DEGREE is
	begin
		if cond then
			return value1;
		else
			return value2;
		end if;
	end function;

	-- ===========================================================================
	-- Random Numbers
	-- ===========================================================================
	constant MAX_SEED1_VALUE		: positive	:= 2147483562;
	constant MAX_SEED2_VALUE		: positive	:= 2147483398;

	function randGenerateInitialSeed return T_SIM_RAND_SEED is
	begin
		return (
			Seed1 => 5,
			Seed2 => 3423
		);
	end function;

	function randBoundSeed(SeedValue : in T_SIM_RAND_SEED) return T_SIM_RAND_SEED is
	begin
		return (
			Seed1	=> (SeedValue.Seed1 - 1 mod MAX_SEED1_VALUE) + 1,
			Seed2	=> (SeedValue.Seed2 - 1 mod MAX_SEED2_VALUE) + 1
		);
	end function;

	procedure randInitializeSeed(Seed : inout T_SIM_RAND_SEED) is
	begin
		Seed	:= randGenerateInitialSeed;
	end procedure;

	procedure randInitializeSeed(Seed : inout T_SIM_RAND_SEED; SeedValue : in T_SIM_RAND_SEED) is
	begin
		Seed	:= randBoundSeed(SeedValue);
	end procedure;

	procedure randInitializeSeed(Seed : inout T_SIM_RAND_SEED; SeedVector : in T_INTVEC) is
	begin
		if (SeedVector'length = 0) then
			Seed	:= randGenerateInitialSeed;
		elsif (SeedVector'length = 1) then
			Seed	:= randBoundSeed(T_SIM_RAND_SEED'(
				Seed1 => SeedVector(0),
				Seed2 => 92346
			));
		elsif (SeedVector'length = 2) then
			Seed	:= randBoundSeed(T_SIM_RAND_SEED'(
				Seed1 => SeedVector(0),
				Seed2 => SeedVector(1)
			));
		else
			-- FIXME:
			-- Seed.Seed1	:= SeedVector(0);
			-- Seed.Seed2	:= SeedVector(1);
		end if;
	end procedure;

	procedure randInitializeSeed(Seed : inout T_SIM_RAND_SEED; SeedVector : in string) is
	begin
		if (SeedVector'length = 0) then
			Seed	:= randGenerateInitialSeed;
		elsif (SeedVector'length = 1) then
			Seed	:= T_SIM_RAND_SEED'(
				Seed1 => character'pos(SeedVector(1)),
				Seed2 => 39834
			);
		elsif (SeedVector'length = 2) then
			Seed	:= T_SIM_RAND_SEED'(
				Seed1 => character'pos(SeedVector(1)),
				Seed2 => character'pos(SeedVector(2))
			);
		else
			-- FIXME:
			-- Seed.Seed1	:= CHARACTER'pos(SeedVector(0));
			-- Seed.Seed2	:= CHARACTER'pos(SeedVector(1));
		end if;
	end procedure;

	function randInitializeSeed return T_SIM_RAND_SEED is
	begin
		return randGenerateInitialSeed;
	end function;

	function randInitializeSeed(SeedValue : T_SIM_RAND_SEED) return T_SIM_RAND_SEED is
	begin
		return randBoundSeed(SeedValue);
	end function;

	function randInitializeSeed(SeedVector : T_INTVEC) return T_SIM_RAND_SEED is
		variable Result		: T_SIM_RAND_SEED;
	begin
		randInitializeSeed(Result, SeedVector);
		return Result;
	end function;

	function randInitializeSeed(SeedVector : string) return T_SIM_RAND_SEED is
		variable Result		: T_SIM_RAND_SEED;
	begin
		randInitializeSeed(Result, SeedVector);
		return Result;
	end function;

	-- ===========================================================================
	-- Uniform distributed random values
	-- ===========================================================================
	procedure randUniformDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL) is
	begin
		ieee.math_real.Uniform(Seed.Seed1, Seed.Seed2, Value);
	end procedure;

	procedure randUniformDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out integer; Minimum : integer; Maximum : integer) is
		variable rand : REAL;
	begin
		if Maximum < Minimum then			report "randUniformDistributedValue: Maximum must be greater than Minimum."	severity FAILURE;		end if;
		ieee.math_real.Uniform(Seed.Seed1, Seed.Seed2, rand);
		Value := scale(rand, Minimum, Maximum);
	end procedure;

	procedure randUniformDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; Minimum : REAL; Maximum : REAL) is
		variable rand : REAL;
	begin
		if Maximum < Minimum then			report "randUniformDistributedValue: Maximum must be greater than Minimum."	severity FAILURE;		end if;
		ieee.math_real.Uniform(Seed.Seed1, Seed.Seed2, rand);
		Value := scale(rand, Minimum, Maximum);
	end procedure;

	-- ===========================================================================
	-- Normal / Gaussian distributed random values
	-- ===========================================================================
	procedure randNormalDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; StandardDeviation : REAL := 1.0; Mean : REAL := 0.0) is
		variable rand1 : REAL;
		variable rand2 : REAL;
	begin
		if StandardDeviation < 0.0 then	report "randNormalDistributedValue: Standard deviation must be >= 0.0"			severity FAILURE;		end if;
		-- Box Muller transformation
		ieee.math_real.Uniform(Seed.Seed1, Seed.Seed2, rand1);
		ieee.math_real.Uniform(Seed.Seed1, Seed.Seed2, rand2);
		--													standard normal distribution: mean 0, variance 1
		Value := StandardDeviation * (sqrt(-2.0 * log(rand1)) * cos(MATH_2_PI * rand2)) + Mean;
	end procedure;

	procedure randNormalDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out integer; StandardDeviation : in REAL; Mean : in REAL; Minimum : in integer; Maximum : in integer) is
		variable rand_real		: REAL;
		variable rand_int			: integer;
	begin
		if Maximum < Minimum then			report "randNormalDistributedValue: Maximum must be greater than Minimum."	severity FAILURE;		end if;
		if StandardDeviation < 0.0 then	report "randNormalDistributedValue: Standard deviation must be >= 0.0"			severity FAILURE;		end if;
		while TRUE loop
			randNormalDistributedValue(Seed, rand_real, StandardDeviation, Mean);
			rand_int	:= integer(round(rand_real));
			exit when ((Minimum <= rand_int) and (rand_int <= Maximum));
		end loop;
		Value := rand_int;
	end procedure;

	procedure randNormalDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; StandardDeviation : in REAL; Mean : in REAL; Minimum : in REAL; Maximum : in REAL) is
		variable rand		: REAL;
	begin
		if Maximum < Minimum then			report "randNormalDistributedValue: Maximum must be greater than Minimum."	severity FAILURE;		end if;
		if StandardDeviation < 0.0 then	report "randNormalDistributedValue: Standard deviation must be >= 0.0"			severity FAILURE;		end if;
		while TRUE loop
			randNormalDistributedValue(Seed, rand, StandardDeviation, Mean);
			exit when ((Minimum <= rand) and (rand <= Maximum));
		end loop;
		Value := rand;
	end procedure;

	-- ===========================================================================
	-- Poisson distributed random values
	-- ===========================================================================
	procedure randPoissonDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; Mean : in REAL) is
		variable Product	: Real;
		variable Bound		: Real;
		variable rand			: Real;
		variable Result		: Real;
	begin
		Product	:= 1.0;
		Result	:= 0.0;
		Bound		:= exp(-1.0 * Mean);
		if ((Mean <= 0.0) or (Bound <= 0.0)) then
			report "randPoissonDistributedValue: Mean must be greater than 0.0." severity FAILURE;
			return;
		end if;

		while (Product >= Bound) loop
			ieee.math_real.Uniform(Seed.Seed1, Seed.Seed2, rand);
			Product		:= Product * rand;
			Result		:= Result + 1.0;
		end loop;
		Value	:= Result;
	end procedure;

	procedure randPoissonDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out integer; Mean : in REAL; Minimum : in integer; Maximum : in integer) is
		variable rand_real		: REAL;
		variable rand_int			: integer;
	begin
		if Maximum < Minimum then			report "randPoissonDistributedValue: Maximum must be greater than Minimum."	severity FAILURE;		end if;
		while TRUE loop
			randPoissonDistributedValue(Seed, rand_real, Mean);
			rand_int	:= integer(round(rand_real));
			exit when ((Minimum <= rand_int) and (rand_int <= Maximum));
		end loop;
		Value := rand_int;
	end procedure;

	procedure randPoissonDistributedValue(Seed : inout T_SIM_RAND_SEED; Value : out REAL; Mean : in REAL; Minimum : in REAL; Maximum : in REAL) is
		variable rand		: REAL;
	begin
		if Maximum < Minimum then			report "randPoissonDistributedValue: Maximum must be greater than Minimum."	severity FAILURE;		end if;
		while TRUE loop
			randPoissonDistributedValue(Seed, rand, Mean);
			exit when ((Minimum <= rand) and (rand <= Maximum));
		end loop;
		Value := rand;
	end procedure;
end package body;
