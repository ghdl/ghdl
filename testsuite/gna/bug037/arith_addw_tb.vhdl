-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- ============================================================================
-- Authors:					Thomas B. Preusser
--
-- Testbench:				arith_addw_tb
--
-- Description:
-- ------------
--   Testbench for arith_addw.
--
-- License:
-- ============================================================================
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

library	IEEE;
use			IEEE.std_logic_1164.all;
use			IEEE.numeric_std.all;

library	PoC;
use			PoC.strings.all;
use			PoC.physical.all;
use			PoC.arith.all;
-- simulation only packages
use			PoC.sim_global.all;
use			PoC.sim_types.all;
use			PoC.simulation.all;


entity arith_addw_tb is
end entity;


architecture tb of arith_addw_tb is
	constant CLOCK_FREQ							: FREQ					:= 100 MHz;

  -- component generics
  constant N : positive := 9;
  constant K : positive := 2;

	subtype tArch_test is tArch;
	subtype tSkip_test is tSkipping;
	
  -- component ports
  subtype word is std_logic_vector(N-1 downto 0);
  type word_vector is array(tArch_test, tSkip_test, boolean) of word;
  type carry_vector is array(tArch_test, tSkip_test, boolean) of std_logic;

	signal Clock	: STD_LOGIC;
  signal a, b : word;
  signal cin  : std_logic;
  signal s    : word_vector;
  signal cout : carry_vector;

begin
	-- initialize global simulation status
	simInitialize;
	
	-- generate global testbench clock
	simGenerateClock(Clock, CLOCK_FREQ);

  -- DUTs
  genArchs: for i in tArch_test generate
    genSkips: for j in tSkip_test generate
      genIncl: for p in false to true generate
				constant simTestID : T_SIM_TEST_ID		:= simCreateTest("Test setup: " &
					"ARCH=" &					str_lalign(TARCH'image(i), 5) &
					"SKIPPING=" &			str_lalign(TSKIPPING'image(j), 8) &
					"P_INCLUSIVE=" &	str_lalign(BOOLEAN'image(p), 7));
			begin
        DUT : entity PoC.arith_addw
          generic map (
            N           => N,
            K           => K,
            ARCH        => i,
            SKIPPING    => j,
            P_INCLUSIVE => p
          )
          port map (
            a    => a,
            b    => b,
            cin  => cin,
            s    => s(i, j, p),
            cout => cout(i, j, p)
          );
      end generate genIncl;
    end generate;
  end generate;

  -- Stimuli
  procChecker : process
		-- from Simulation
		constant simProcessID	: T_SIM_PROCESS_ID := simRegisterProcess("Combined Generator and Checker");	--, "aaa/bbb/ccc");	--globalSimulationStatus'instance_name);
		
  begin
    for i in natural range 0 to 2**N-1 loop
      a <= std_logic_vector(to_unsigned(i, N));
      for j in natural range 0 to 2**N-1 loop
        b <= std_logic_vector(to_unsigned(j, N));

        cin <= '0';
        wait until rising_edge(Clock);
        for arch in tArch_test loop
					for skip in tSkip_test loop
						for incl in boolean loop
							simAssertion((i+j) mod 2**(N+1) = to_integer(unsigned(cout(arch, skip, incl) & s(arch, skip, incl))),
								  "Output Error["&tArch'image(arch)&','&tSkipping'image(skip)&','&boolean'image(incl)&"]: "&
								  integer'image(i)&'+'&integer'image(j)&" != "&
								  integer'image(to_integer(unsigned(cout(arch, skip, incl) & s(arch, skip, incl)))));
						end loop;
					end loop;
        end loop;
        
        cin <= '1';
        wait until falling_edge(Clock);
        for arch in tArch_test loop
					for skip in tSkip_test loop
						for incl in boolean loop
							simAssertion((i+j+1) mod 2**(N+1) = to_integer(unsigned(cout(arch, skip, incl) & s(arch, skip, incl))),
								  "Output Error["&tArch'image(arch)&','&tSkipping'image(skip)&','&boolean'image(incl)&"]: "&
								  integer'image(i)&'+'&integer'image(j)&"+1 != "&
								  integer'image(to_integer(unsigned(cout(arch, skip, incl) & s(arch, skip, incl)))));
						end loop;
					end loop;
        end loop;

      end loop;  -- j
    end loop;  -- i
		
    -- This process is finished
		simDeactivateProcess(simProcessID);
		-- Report overall result
		simFinalize;
		wait;  -- forever
  end process;

end architecture;
