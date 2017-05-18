-- EMACS settings: -*-  tab-width: 2;indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2;replace-tabs off;indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
--
-- Package:     		Protected type implementations.
--
-- Description:
-- -------------------------------------
-- .. TODO:: No documentation available.
--
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany,
--  					 				 Chair of VLSI-Design, Diagnostics and Architecture
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
use			IEEE.math_real.all;

library	PoC;
-- use			PoC.my_project.all;
-- use			PoC.utils.all;


package ProtectedTypes is
	-- protected BOOLEAN implementation
	-- ===========================================================================
	type P_BOOLEAN is protected
		procedure				Clear;
		procedure				Set(Value : boolean := TRUE);
		impure function	Get return boolean;
		impure function Toggle return boolean;
	end protected;

	-- protected INTEGER implementation
	-- ===========================================================================
	-- TODO: Mult, Div, Pow, Mod, Rem
	type P_INTEGER is protected
		procedure				Clear;
		procedure				Set(Value : integer);
		impure function	Get return integer;
		procedure				Add(Value : integer);
		impure function Add(Value : integer) return integer;
		procedure				Sub(Value : integer);
		impure function Sub(Value : integer) return integer;
	end protected;

	-- protected NATURAL implementation
	-- ===========================================================================
	-- TODO: Mult, Div, Pow, Mod, Rem
	type P_NATURAL is protected
		procedure				Clear;
		procedure				Set(Value : natural);
		impure function	Get return natural;
		procedure				Add(Value : natural);
		impure function Add(Value : natural) return natural;
		procedure				Sub(Value : natural);
		impure function Sub(Value : natural) return natural;
	end protected;

	-- protected POSITIVE implementation
	-- ===========================================================================
	-- TODO: Mult, Div, Pow, Mod, Rem
	type P_POSITIVE is protected
		procedure				Clear;
		procedure				Set(Value : positive);
		impure function	Get return positive;
		procedure				Add(Value : positive);
		impure function Add(Value : positive) return positive;
		procedure				Sub(Value : positive);
		impure function Sub(Value : positive) return positive;
	end protected;

	-- protected REAL implementation
	-- ===========================================================================
	-- TODO: Round, Mult, Div, Pow, Mod
	type P_REAL is protected
		procedure				Clear;
		procedure				Set(Value : REAL);
		impure function	Get return REAL;
		procedure				Add(Value : REAL);
		impure function Add(Value : REAL) return REAL;
		procedure				Sub(Value : REAL);
		impure function Sub(Value : REAL) return REAL;
	end protected;
end package;


package body ProtectedTypes is
	-- protected BOOLEAN implementation
	-- ===========================================================================
	type P_BOOLEAN is protected body
		variable InnerValue		: boolean		:= FALSE;

		procedure Clear is
		begin
			InnerValue	:= FALSE;
		end procedure;

		procedure Set(Value : boolean := TRUE) is
		begin
			InnerValue	:= Value;
		end procedure;

		impure function Get return boolean is
		begin
			return InnerValue;
		end function;

		impure function Toggle return boolean is
		begin
			InnerValue	:= not InnerValue;
			return InnerValue;
		end function;
	end protected body;

	-- protected INTEGER implementation
	-- ===========================================================================
	type P_INTEGER is protected body
		variable InnerValue		: integer		:= 0;

		procedure Clear is
		begin
			InnerValue	:= 0;
		end procedure;

		procedure Set(Value : integer) is
		begin
			InnerValue	:= Value;
		end procedure;

		impure function Get return integer is
		begin
			return InnerValue;
		end function;

		procedure Add(Value : integer) is
		begin
			InnerValue	:= InnerValue + Value;
		end procedure;

		impure function Add(Value : integer) return integer is
		begin
			Add(Value);
			return InnerValue;
		end function;

		procedure Sub(Value : integer) is
		begin
			InnerValue	:= InnerValue - Value;
		end procedure;

		impure function Sub(Value : integer) return integer is
		begin
			Sub(Value);
			return InnerValue;
		end function;
	end protected body;

	-- protected NATURAL implementation
	-- ===========================================================================
	type P_NATURAL is protected body
		variable InnerValue		: natural		:= 0;

		procedure Clear is
		begin
			InnerValue	:= 0;
		end procedure;

		procedure Set(Value : natural) is
		begin
			InnerValue	:= Value;
		end procedure;

		impure function Get return natural is
		begin
			return InnerValue;
		end function;

		procedure Add(Value : natural) is
		begin
			InnerValue	:= InnerValue + Value;
		end procedure;

		impure function Add(Value : natural) return natural is
		begin
			Add(Value);
			return InnerValue;
		end function;

		procedure Sub(Value : natural) is
		begin
			InnerValue	:= InnerValue - Value;
		end procedure;

		impure function Sub(Value : natural) return natural is
		begin
			Sub(Value);
			return InnerValue;
		end function;
	end protected body;

	-- protected POSITIVE implementation
	-- ===========================================================================
	type P_POSITIVE is protected body
		variable InnerValue		: positive		:= 1;

		procedure Clear is
		begin
			InnerValue	:= 1;
		end procedure;

		procedure Set(Value : positive) is
		begin
			InnerValue	:= Value;
		end procedure;

		impure function Get return positive is
		begin
			return InnerValue;
		end function;

		procedure Add(Value : positive) is
		begin
			InnerValue	:= InnerValue + Value;
		end procedure;

		impure function Add(Value : positive) return positive is
		begin
			Add(Value);
			return InnerValue;
		end function;

		procedure Sub(Value : positive) is
		begin
			InnerValue	:= InnerValue - Value;
		end procedure;

		impure function Sub(Value : positive) return positive is
		begin
			Sub(Value);
			return InnerValue;
		end function;
	end protected body;

	-- protected REAL implementation
	-- ===========================================================================
	type P_REAL is protected body
		variable InnerValue		: REAL		:= 0.0;

		procedure Clear is
		begin
			InnerValue	:= 0.0;
		end procedure;

		procedure Set(Value : REAL) is
		begin
			InnerValue	:= Value;
		end procedure;

		impure function Get return REAL is
		begin
			return InnerValue;
		end function;

		procedure Add(Value : REAL) is
		begin
			InnerValue	:= InnerValue + Value;
		end procedure;

		impure function Add(Value : REAL) return REAL is
		begin
			Add(Value);
			return InnerValue;
		end function;

		procedure Sub(Value : REAL) is
		begin
			InnerValue	:= InnerValue - Value;
		end procedure;

		impure function Sub(Value : REAL) return REAL is
		begin
			Sub(Value);
			return InnerValue;
		end function;
	end protected body;
end package body;
