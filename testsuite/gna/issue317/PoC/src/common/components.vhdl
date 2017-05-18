-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:				 	Patrick Lehmann
--
-- Package:				 	Common primitives described as a function
--
-- Description:
-- -------------------------------------
--		This packages describes common primitives like flip flops and multiplexers
--		as a function to use them as one-liners.
--
--	ATTENSION:
--		The parameter 'constant INIT' of some functions is actually the reset
--		value, not the initial value after device programming (e.g. for FPGAs),
--		this value MUST be set via signal declaration!
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

library PoC;
use			PoC.utils.all;


package components is
	-- implement an optional register stage
	function registered(signal Clock : std_logic; constant IsRegistered : boolean) return boolean;

	-- FlipFlop functions
	-- ===========================================================================
	-- RS-FlipFlops
	function ffrs(q : std_logic;	rst : std_logic := '0'; set : std_logic := '0') return std_logic;				-- RS-FlipFlop with dominant rst
	function ffsr(q : std_logic;	rst : std_logic := '0'; set : std_logic := '0') return std_logic;				-- RS-FlipFlop with dominant set
	-- D-FlipFlops (Delay)
	function ffdre(q : std_logic;					d : std_logic;				rst : std_logic := '0'; en : std_logic := '1'; constant INIT : std_logic := '0')												return std_logic;					-- D-FlipFlop with reset and enable
	function ffdre(q : std_logic_vector;	d : std_logic_vector;	rst : std_logic := '0'; en : std_logic := '1'; constant INIT : std_logic_vector := (0 to 0 => '0'))	return std_logic_vector;	-- D-FlipFlop with reset and enable
	function ffdse(q : std_logic;					d : std_logic;				set : std_logic := '0'; en : std_logic := '1')																													return std_logic;					-- D-FlipFlop with set and enable
	-- T-FlipFlops (Toggle)
	function fftre(q : std_logic;					t : std_logic;				rst : std_logic := '0'; en : std_logic := '1'; constant INIT : std_logic := '0')												return std_logic;					-- T-FlipFlop with reset and enable
	function fftse(q : std_logic;					t : std_logic;				set : std_logic := '0'; en : std_logic := '1')																													return std_logic;					-- T-FlipFlop with set and enable

	-- counter
	function upcounter_next(cnt : unsigned; rst : std_logic := '0'; en : std_logic := '1'; constant INIT : natural := 0) return unsigned;
	function upcounter_equal(cnt : unsigned; value : natural) return std_logic;
	function downcounter_next(cnt : signed; rst : std_logic := '0'; en : std_logic := '1'; constant INIT : integer := 0) return signed;
	function downcounter_equal(cnt : signed; value : integer) return std_logic;
	function downcounter_neg(cnt : signed) return std_logic;

	-- shiftregisters
	function shreg_left(q : std_logic_vector; i : std_logic; en : std_logic := '1') return std_logic_vector;
	function shreg_right(q : std_logic_vector; i : std_logic; en : std_logic := '1') return std_logic_vector;
	-- rotate registers
	function rreg_left(q : std_logic_vector; en : std_logic := '1') return std_logic_vector;
	function rreg_right(q : std_logic_vector; en : std_logic := '1') return std_logic_vector;

	-- compare
	function comp(value1 : std_logic_vector; value2 : std_logic_vector) return std_logic_vector;
	function comp(value1 : unsigned; value2 : unsigned) return unsigned;
	function comp(value1 : signed; value2 : signed) return signed;
	function comp_allzero(value	: std_logic_vector)	return std_logic;
	function comp_allzero(value	: unsigned)					return std_logic;
	function comp_allzero(value	: signed)						return std_logic;
	function comp_allone(value	: std_logic_vector)	return std_logic;
	function comp_allone(value	: unsigned)					return std_logic;
	function comp_allone(value	: signed)						return std_logic;

	-- multiplexing
	function mux(sel : std_logic; sl0		: std_logic;				sl1		: std_logic)				return std_logic;
	function mux(sel : std_logic; slv0	: std_logic_vector;	slv1	: std_logic_vector)	return std_logic_vector;
	function mux(sel : std_logic; us0		: unsigned;					us1		: unsigned)					return unsigned;
	function mux(sel : std_logic; s0		: signed;						s1		: signed)						return signed;
end package;


package body components is
	-- implement an optional register stage
	-- ===========================================================================
	function registered(signal Clock : std_logic; constant IsRegistered : boolean) return boolean is
	begin
		return ite(IsRegistered, rising_edge(Clock), TRUE);
	end function;

	-- FlipFlops
	-- ===========================================================================
	-- D-flipflop with reset and enable
	function ffdre(q : std_logic; d : std_logic; rst : std_logic := '0'; en : std_logic := '1'; constant INIT : std_logic := '0') return std_logic is
	begin
		if not SIMULATION then
			if (INIT = '0') then
				return ((d and en) or (q and not en)) and not rst;
			elsif (INIT = '1') then
				return ((d and en) or (q and not en)) or rst;
			else
				report "Unsupported INIT value for synthesis." severity FAILURE;
				return 'X';
			end if;
		elsif (rst = '1') then
			return INIT;
		else
			return ((d and en) or (q and not en));
		end if;
	end function;

	function ffdre(q : std_logic_vector; d : std_logic_vector; rst : std_logic := '0'; en : std_logic := '1'; constant INIT : std_logic_vector := (0 to 0 => '0')) return std_logic_vector is
		constant INIT_I		: std_logic_vector(q'range)		:= resize(INIT, q'length);
		variable Result		: std_logic_vector(q'range);
	begin
		for i in q'range loop
			Result(i)		:= ffdre(q => q(i), d => d(i), rst => rst, en => en, INIT => INIT_I(i));
		end loop;
		return Result;
	end function;

	-- D-flipflop with set and enable
	function ffdse(q : std_logic; d : std_logic; set : std_logic := '0'; en : std_logic := '1') return std_logic is
	begin
		return ffdre(q => q, d => d, rst => set, en => en, INIT => '1');
	end function;

	-- T-flipflop with reset and enable
	function fftre(q : std_logic; t : std_logic; rst : std_logic := '0'; en : std_logic := '1'; constant INIT : std_logic := '0') return std_logic is
	begin
		if not SIMULATION then
			if (INIT = '0') then
				return ((not q and (t and en)) or (q and not (t and en))) and not rst;
			elsif (INIT = '1') then
				return ((not q and (t and en)) or (q and not (t and en))) or rst;
			else
				report "Unsupported INIT value for synthesis." severity FAILURE;
				return 'X';
			end if;
		elsif (rst = '1') then
			return INIT;
		else
			return ((not q and (t and en)) or (q and not (t and en)));
		end if;
	end function;

	-- T-flipflop with set and enable
	function fftse(q : std_logic; t : std_logic; set : std_logic := '0'; en : std_logic := '1') return std_logic is
	begin
		return fftre(q => q, t => t, rst => set, en => en, INIT => '1');
	end function;

	-- RS-flipflop with dominant rst
	function ffrs(q : std_logic; rst : std_logic := '0'; set : std_logic := '0') return std_logic is
	begin
		return (q or set) and not rst;
	end function;

	-- RS-flipflop with dominant set
	function ffsr(q : std_logic; rst : std_logic := '0'; set : std_logic := '0') return std_logic is
	begin
		return (q and not rst) or set;
	end function;


	-- Counters
	-- ===========================================================================
	-- up-counter
	function upcounter_next(cnt : unsigned; rst : std_logic := '0'; en : std_logic := '1'; constant INIT : natural := 0) return unsigned is
	begin
		if (rst = '1') then
			return to_unsigned(INIT, cnt'length);
		elsif (en = '1') then
			return cnt + 1;
		else
			return cnt;
		end if;
	end function;

	function upcounter_equal(cnt : unsigned; value : natural) return std_logic is
	begin
		-- optimized comparison for only up counting values
		return to_sl((cnt and to_unsigned(value, cnt'length)) = value);
	end function;

	-- down-counter
	function downcounter_next(cnt : signed; rst : std_logic := '0'; en : std_logic := '1'; constant INIT : integer := 0) return signed is
	begin
		if (rst = '1') then
			return to_signed(INIT, cnt'length);
		elsif (en = '1') then
			return cnt - 1;
		else
			return cnt;
		end if;
	end function;

	function downcounter_equal(cnt : signed; value : integer) return std_logic is
	begin
		-- optimized comparison for only down counting values
		return to_sl((cnt nor to_signed(value, cnt'length)) /= value);
	end function;

	function downcounter_neg(cnt : signed) return std_logic is
	begin
		return cnt(cnt'high);
	end function;

	-- Shift/Rotate Registers
	-- ===========================================================================
	function shreg_left(q : std_logic_vector; i : std_logic; en : std_logic := '1') return std_logic_vector is
	begin
		return mux(en, q, q(q'left - 1 downto q'right) & i);
	end function;

	function shreg_right(q : std_logic_vector; i : std_logic; en : std_logic := '1') return std_logic_vector is
	begin
		return mux(en, q, i & q(q'left downto q'right - 1));
	end function;

	function rreg_left(q : std_logic_vector; en : std_logic := '1') return std_logic_vector is
	begin
		return mux(en, q, q(q'left - 1 downto q'right) & q(q'left));
	end function;

	function rreg_right(q : std_logic_vector; en : std_logic := '1') return std_logic_vector is
	begin
		return mux(en, q, q(q'right) & q(q'left downto q'right - 1));
	end function;

	-- compare functions
	-- ===========================================================================
	-- Returns, when
	--	1-		=> value1 < value2 (difference is negative)
	--	00		=> value1 = value2 (difference is zero)
	--	-1		=> value1 > value2 (difference is positive)
	function comp(value1 : std_logic_vector; value2 : std_logic_vector) return std_logic_vector is
	begin
		report "Comparing two STD_LOGIC_VECTORs - implicit conversion to UNSIGNED" severity WARNING;
		return std_logic_vector(comp(unsigned(value1), unsigned(value2)));
	end function;

	function comp(value1 : unsigned; value2 : unsigned) return unsigned is
	begin
		if value1 < value2 then
			return "10";
		elsif value1 = value2 then
			return "00";
		else
			return "01";
		end if;
	end function;

	function comp(value1 : signed; value2 : signed) return signed is
	begin
		if value1 < value2 then
			return "10";
		elsif value1 = value2 then
			return "00";
		else
			return "01";
		end if;
	end function;

	function comp_allzero(value	: std_logic_vector) return std_logic is
	begin
		return comp_allzero(unsigned(value));
	end function;

	function comp_allzero(value	: unsigned) return std_logic is
	begin
		return to_sl(value = (value'range => '0'));
	end function;

	function comp_allzero(value	: signed) return std_logic is
	begin
		return to_sl(value = (value'range => '0'));
	end function;

	function comp_allone(value	: std_logic_vector) return std_logic is
	begin
		return comp_allone(unsigned(value));
	end function;

	function comp_allone(value	: unsigned) return std_logic is
	begin
		return to_sl(value = (value'range => '1'));
	end function;

	function comp_allone(value	: signed) return std_logic is
	begin
		return to_sl(value = (value'range => '1'));
	end function;


	-- multiplexers
	function mux(sel : std_logic; sl0 : std_logic; sl1 : std_logic) return std_logic is
	begin
		return (sl0 and not sel) or (sl1 and sel);
	end function;

	function mux(sel : std_logic; slv0 : std_logic_vector; slv1 : std_logic_vector) return std_logic_vector is
	begin
		return (slv0 and not (slv0'range => sel)) or (slv1 and (slv1'range => sel));
	end function;

	function mux(sel : std_logic; us0 : unsigned; us1 : unsigned) return unsigned is
	begin
		return (us0 and not (us0'range => sel)) or (us1 and (us1'range => sel));
	end function;

	function mux(sel : std_logic; s0 : signed; s1 : signed) return signed is
	begin
		return (s0 and not (s0'range => sel)) or (s1 and (s1'range => sel));
	end function;
end package body;
