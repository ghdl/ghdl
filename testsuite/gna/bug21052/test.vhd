library std;
use std.textio.all;
entity test is end entity;
architecture a of test is
	component wrapper is end component;
begin
	inst :if false generate inst :wrapper; end generate;
	process
		variable l :line;
	begin
		write(l, string'("OK")); writeline(output, l); wait;
	end process;
end architecture;

package some_package is
	-- this signal seems to be problematic
	signal some_signal :bit;
	component some_component end component;
end package;

entity wrapper is end entity;

architecture a of wrapper is begin
	inst :work.some_package.some_component;
end architecture;



-- ################################################################################
-- $ ghdl -c test.vhd -e test
-- test.vhd:17:8:warning: component instance "inst" is not bound
-- test.vhd:16:14:warning: (in default configuration of wrapper(a))
-- $ ./test
-- OK
-- $ ./test --wave=test.ghw
-- Aborted (core dumped)
-- $ ghdl --version
-- GHDL 0.29 (20100109) [Sokcho edition]
--  Compiled with GNAT Version: 4.7.2
--  GCC back-end code generator
-- Written by Tristan Gingold.
-- 
-- Copyright (C) 2003 - 2010 Tristan Gingold.
-- GHDL is free software, covered by the GNU General Public License.  There is NO
-- warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- ################################################################################
-- $ ghdl -c test.vhd -e test
-- test.vhd:19:8:warning: component instance "inst" is not bound
-- test.vhd:18:14:warning: (in default configuration of wrapper(a))
-- $ ./test
-- OK
-- $ ./test --wave=test.ghw
-- ^C
-- $ ghdl --version
-- GHDL 0.29 (20100109) [Sokcho edition]
--  Compiled with GNAT Version: 4.4.2 20091222 (Red Hat 4.4.2-20
--  GCC back-end code generator
-- Written by Tristan Gingold.
--
-- Copyright (C) 2003 - 2010 Tristan Gingold.
-- GHDL is free software, covered by the GNU General Public License.  There is NO
-- warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- ################################################################################
-- $ ghdl -c test.vhd -e test
-- test.vhd:19:8:warning: component instance "inst" is not bound
-- test.vhd:18:14:warning: (in default configuration of wrapper(a))
-- $ ./test
-- OK
-- $ ./test --wave=test.ghw
--
-- raised CONSTRAINT_ERROR : grt-waves.adb:824 access check failed
-- $ ghdl --version
-- GHDL 0.30dev (20100112) [Sokcho edition]
--  Compiled with GNAT Version: 4.8.0 20130412 (Red Hat 4.8.0-2)
--  GCC back-end code generator
-- Written by Tristan Gingold.
--
-- Copyright (C) 2003 - 2010 Tristan Gingold.
-- GHDL is free software, covered by the GNU General Public License.  There is NO
-- warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- ################################################################################
