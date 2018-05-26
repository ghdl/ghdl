-- This snippet causes an exception:
--	PS H:\Austausch\PoC\temp\bugreport> C:\Tools\GHDL.new\bin\ghdl.exe -a -v .\1_SecondaryUnit.vhd
--	.\1_SecondaryUnit.vhd:19:42: can't match physical literal with type physical type "t_angel"
--	are_trees_equal: cannot handle IIR_KIND_UNIT_DECLARATION (*std_standard*:1:1)
--	
--	******************** GHDL Bug occurred ****************************
--	Please report this bug on https://github.com/tgingold/ghdl/issues
--	GHDL release: GHDL 0.34dev (commit: 2016-01-20;  git branch: paebbels/llvm';  hash: 3a8fd5b) [Dunoon edition]
--	Compiled with GNAT Version: GPL 2015 (20150428-49)
--	In directory: H:\Austausch\PoC\temp\bugreport\
--	Command line:
--	C:\Tools\GHDL.new\bin\ghdl.exe -a -v .\1_SecondaryUnit.vhd
--	Exception TYPES.INTERNAL_ERROR raised
--	Exception information:
--	Exception name: TYPES.INTERNAL_ERROR
--	Message: errorout.adb:66
--	******************************************************************
--
-- The "syntax" error is, that I forgot to rename '10 sec' in line 35

package pkg is
	type T_ANGEL is range INTEGER'low to INTEGER'high units
		second;
		minute	= 60 second;
		deg			= 60 minute;
	end units;
	
	subtype T_PHASE is T_ANGEL range	-360 deg to 360 deg;
	
	function test1(Phase : T_PHASE := 10 second) return T_PHASE;
	procedure test2(signal output : out T_PHASE; input : T_PHASE := 10.0 second);
end package;
	
package body pkg is
	function test1(Phase : T_PHASE := 10 sec) return T_PHASE is
	begin
		return Phase + 1.0 deg;
	end function;
	
	procedure test2(signal output : out T_PHASE; input : T_PHASE := 10.0 second) is
	begin
		output <= input;
	end procedure;
end package body;


use			works.pkg.all;

entity SecondaryUnit_tb is
end entity;

architecture test of SecondaryUnit_tb is
	signal TestSignal1 : T_PHASE;
	signal TestSignal2 : T_PHASE;
begin
	TestSignal1	<= test1(50.0 second);
	test2(TestSignal2, TestSignal1);
end architecture;
