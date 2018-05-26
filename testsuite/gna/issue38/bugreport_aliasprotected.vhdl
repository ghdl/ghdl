-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Patrick Lehmann
-- Reproducer:			Using aliases to protected type methods cause an exception.
-- 
-- License:
-- =============================================================================
-- Copyright 2007-2016 Technische Universitaet Dresden - Germany
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
--
-- Issue 1:
--	When analyzed in VHDL-93 mode an error is reported:
--		.\bugreport_aliasprotected.vhdl:4:26: protected type not allowed in vhdl87/93
--		.\bugreport_aliasprotected.vhdl:9:12: 'protected' is expected instead of 'protected'
--	Line 1 is perfectly clear, but what is the intension of line 2?
--	Is this follow up error necessary or should it have another message text?
--
-- Issue 2:
--	Calling an aliases to a shared variable's method causes an exception in GHDL:
--		******************** GHDL Bug occurred ****************************
--		Please report this bug on https://github.com/tgingold/ghdl/issues
--		GHDL release: GHDL 0.34dev (commit: 2016-01-27;  git branch: paebbels/master';  hash: d424eb8) [Dunoon edition]
--		Compiled with GNAT Version: GPL 2015 (20150428-49)
--		In directory: H:\Austausch\PoC\temp\ghdl\
--		Command line:
--		C:\Tools\GHDL.new\bin\ghdl.exe -r --std=08 test
--		Exception TYPES.INTERNAL_ERROR raised
--		Exception information:
--		Exception name: TYPES.INTERNAL_ERROR
--		Message: trans.adb:487
--		******************************************************************
--	The alias definition by itself is not causing any errors. In my big example, I
--	could at least use an alias to a procedure without parameters. This short example
--	throws exceptions on all 4 variants (with/without parameter; with/without return value).
--
--	You can comment/uncomment the alias/wrapping function/procedure to cause the error.
--
-- GHDL calls:
--	PS> ghdl.exe -a --std=08 .\bugreport_aliasprotected.vhdl
--	PS> ghdl.exe -r --std=08 test
--
-- Expected output:
--	.\bugreport_aliasprotected.vhdl:163:16:@0ms:(report note): wrapGet:      7  expected: 7
--	.\bugreport_aliasprotected.vhdl:165:16:@0ms:(report note): wrapGet:      5  expected: 5
--	.\bugreport_aliasprotected.vhdl:166:16:@0ms:(report note): wrapExcahnge: 5  expected: 5
--	.\bugreport_aliasprotected.vhdl:167:16:@0ms:(report note): wrapGet:      3  expected: 3
--	.\bugreport_aliasprotected.vhdl:169:16:@0ms:(report note): wrapGet:      0  expected: 0
--	
-- =============================================================================
-- Protected type package
-- =============================================================================
package pkg is
	type T_INTEGER is protected
		procedure				Clear;
		procedure				Set(Value : INTEGER);
		impure function	Get return INTEGER;
		impure function	Exchange(Value : INTEGER) return INTEGER;
	end protected;
end package;

package body pkg is
	type T_INTEGER is protected body
		variable LocalVariable	: INTEGER		:= 7;
		
		procedure Clear is
		begin
			LocalVariable		:= 0;
		end procedure;
		
		procedure Set(Value : INTEGER) is
		begin
			LocalVariable		:= Value;
		end procedure;
		
		impure function Get return INTEGER is
		begin
			return LocalVariable;
		end function;
		
		impure function	Exchange(Value : INTEGER) return INTEGER is
			variable Result	: INTEGER;
		begin
			Result				:= LocalVariable;
			LocalVariable	:= Value;
			return Result;
		end function;
	end protected body;
end package body;

-- =============================================================================
-- Wrapper package
-- =============================================================================
use work.pkg.all;

package wrapper is
	shared variable MyBoolean		: T_INTEGER;
	
	-- alias						wrapClear			is MyBoolean.Clear[];				-- if this alias is used, GHDL crashes
	alias						wrapperClear	is MyBoolean.Clear[];				-- unused alias => no crash
	procedure				wrapClear;																-- wrapped by a call chain => no crash
	
	-- alias						wrapSet				is MyBoolean.Set[INTEGER];
	procedure				wrapSet(Value : INTEGER);
	
	-- alias						wrapGet				is MyBoolean.Get[return INTEGER];
	impure function	wrapGet return INTEGER;
	
	-- alias						wrapExchange	is MyBoolean.Exchange[INTEGER return INTEGER];
	impure function	wrapExchange(Value : INTEGER) return INTEGER;
	
end package;

package body wrapper is
	procedure wrapClear is
	begin
		MyBoolean.Clear;
	end procedure;
	
	procedure wrapSet(Value : INTEGER) is
	begin
		MyBoolean.Set(Value);
	end procedure;
	
	impure function wrapGet return INTEGER is
	begin
		return MyBoolean.Get;
	end function;
	
	impure function	wrapExchange(Value : INTEGER) return INTEGER is
	begin
		return MyBoolean.Exchange(Value);
	end function;
end package body;

-- =============================================================================
-- Testbench
-- =============================================================================
use work.wrapper.all;

entity test is
end entity;


architecture tb of test is
begin
	process
	begin
		report "wrapGet:      " & INTEGER'image(wrapGet)					& "  expected: 7" severity NOTE;
		wrapSet(5);
		report "wrapGet:      " & INTEGER'image(wrapGet)					& "  expected: 5" severity NOTE;
		report "wrapExcahnge: " & INTEGER'image(wrapExchange(3))	& "  expected: 5" severity NOTE;
		report "wrapGet:      " & INTEGER'image(wrapGet)					& "  expected: 3" severity NOTE;
		wrapperClear;
		report "wrapGet:      " & INTEGER'image(wrapGet)					& "  expected: 0" severity NOTE;
		wait;
	end process;
end architecture;
