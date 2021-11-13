-- #################################################################################################
-- # << NEORV32 - Main VHDL package file >>                                                        #
-- # ********************************************************************************************* #
-- # BSD 3-Clause License                                                                          #
-- #                                                                                               #
-- # Copyright (c) 2021, Stephan Nolting. All rights reserved.                                     #
-- #                                                                                               #
-- # Redistribution and use in source and binary forms, with or without modification, are          #
-- # permitted provided that the following conditions are met:                                     #
-- #                                                                                               #
-- # 1. Redistributions of source code must retain the above copyright notice, this list of        #
-- #    conditions and the following disclaimer.                                                   #
-- #                                                                                               #
-- # 2. Redistributions in binary form must reproduce the above copyright notice, this list of     #
-- #    conditions and the following disclaimer in the documentation and/or other materials        #
-- #    provided with the distribution.                                                            #
-- #                                                                                               #
-- # 3. Neither the name of the copyright holder nor the names of its contributors may be used to  #
-- #    endorse or promote products derived from this software without specific prior written      #
-- #    permission.                                                                                #
-- #                                                                                               #
-- # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS   #
-- # OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF               #
-- # MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE    #
-- # COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,     #
-- # EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE #
-- # GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED    #
-- # AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     #
-- # NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED  #
-- # OF THE POSSIBILITY OF SUCH DAMAGE.                                                            #
-- # ********************************************************************************************* #
-- # The NEORV32 Processor - https://github.com/stnolting/neorv32              (c) Stephan Nolting #
-- #################################################################################################

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package neorv32_package is

  -- Architecture Configuration -------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  -- address space --
  constant ispace_base_c : std_ulogic_vector(31 downto 0) := x"00000000"; -- default instruction memory address space base address
  constant dspace_base_c : std_ulogic_vector(31 downto 0) := x"80000000"; -- default data memory address space base address

  -- CPU core --
  constant dedicated_reset_c : boolean := false; -- use dedicated hardware reset value for UNCRITICAL registers (FALSE=reset value is irrelevant (might simplify HW), default; TRUE=defined LOW reset value)
  constant cp_timeout_en_c   : boolean := false; -- auto-terminate pending co-processor operations after 256 cycles (for debugging only), default = false

  -- "critical" number of implemented PMP regions --
  -- if more PMP regions (> pmp_num_regions_critical_c) are defined, another register stage is automatically inserted into the memory interfaces
  -- increasing instruction fetch & data access latency by +1 cycle but also reducing critical path length
  constant pmp_num_regions_critical_c : natural := 8; -- default=8

  -- "response time window" for processor-internal memories and IO devices
  constant max_proc_int_response_time_c : natural := 15; -- cycles after which an *unacknowledged* internal bus access will timeout and trigger a bus fault exception (min 2)

  -- jtag tap - identifier --
  constant jtag_tap_idcode_version_c : std_ulogic_vector(03 downto 0) := x"0"; -- version
  constant jtag_tap_idcode_partid_c  : std_ulogic_vector(15 downto 0) := x"cafe"; -- part number
  constant jtag_tap_idcode_manid_c   : std_ulogic_vector(10 downto 0) := "00000000000"; -- manufacturer id

  -- Architecture Constants (do not modify!) ------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  constant data_width_c : natural := 32; -- native data path width - do not change!
  constant hw_version_c : std_ulogic_vector(31 downto 0) := x"01060306"; -- no touchy!
  constant archid_c     : natural := 19; -- official NEORV32 architecture ID - hands off!

  -- External Interface Types ---------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  type sdata_8x32_t  is array (0 to 7)  of std_ulogic_vector(31 downto 0);
  type sdata_8x32r_t is array (0 to 7)  of std_logic_vector(31 downto 0); -- resolved type

  -- Internal Interface Types ---------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  type pmp_ctrl_if_t is array (0 to 63) of std_ulogic_vector(07 downto 0);
  type pmp_addr_if_t is array (0 to 63) of std_ulogic_vector(33 downto 0);
  type cp_data_if_t  is array (0 to 3)  of std_ulogic_vector(data_width_c-1 downto 0);

  -- Internal Memory Types Configuration Types ----------------------------------------------
  -- -------------------------------------------------------------------------------------------
  type mem32_t is array (natural range <>) of std_ulogic_vector(31 downto 0); -- memory with 32-bit entries
  type mem8_t  is array (natural range <>) of std_ulogic_vector(07 downto 0); -- memory with 8-bit entries

  -- Helper Functions -----------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  function index_size_f(input : natural) return natural;
  function is_power_of_two_f(input : natural) return boolean;

end neorv32_package;

package body neorv32_package is

  -- Function: Minimal required number of bits to represent input number --------------------
  -- -------------------------------------------------------------------------------------------
  function index_size_f(input : natural) return natural is
  begin
    for i in 0 to natural'high loop
      if (2**i >= input) then
        return i;
      end if;
    end loop; -- i
    return 0;
  end function index_size_f;

  -- Function: Test if input number is a power of two ---------------------------------------
  -- -------------------------------------------------------------------------------------------
  function is_power_of_two_f(input : natural) return boolean is
  begin
    if (input = 1) then -- 2^0
      return true;
    elsif ((input / 2) /= 0) and ((input mod 2) = 0) then
      return true;
    else
      return false;
    end if;
  end function is_power_of_two_f;
end neorv32_package;
