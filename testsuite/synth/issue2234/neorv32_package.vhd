-- #################################################################################################
-- # << NEORV32 - Main VHDL package file >>                                                        #
-- # ********************************************************************************************* #
-- # BSD 3-Clause License                                                                          #
-- #                                                                                               #
-- # Copyright (c) 2022, Stephan Nolting. All rights reserved.                                     #
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


  -- Helper Functions -----------------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  function index_size_f(input : natural) return natural;
  function or_reduce_f(a : std_ulogic_vector) return std_ulogic;
  function and_reduce_f(a : std_ulogic_vector) return std_ulogic;
  function xor_reduce_f(a : std_ulogic_vector) return std_ulogic;
  function bit_rev_f(input : std_ulogic_vector) return std_ulogic_vector;
  function is_power_of_two_f(input : natural) return boolean;


end neorv32_package;

package body neorv32_package is

  -- Function: Minimal required number of bits to represent <input> numbers -----------------
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


  -- Function: OR-reduce all bits -----------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  function or_reduce_f(a : std_ulogic_vector) return std_ulogic is
    variable tmp_v : std_ulogic;
  begin
    tmp_v := '0';
    for i in a'range loop
      tmp_v := tmp_v or a(i);
    end loop; -- i
    return tmp_v;
  end function or_reduce_f;

  -- Function: AND-reduce all bits ----------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  function and_reduce_f(a : std_ulogic_vector) return std_ulogic is
    variable tmp_v : std_ulogic;
  begin
    tmp_v := '1';
    for i in a'range loop
      tmp_v := tmp_v and a(i);
    end loop; -- i
    return tmp_v;
  end function and_reduce_f;

  -- Function: XOR-reduce all bits ----------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  function xor_reduce_f(a : std_ulogic_vector) return std_ulogic is
    variable tmp_v : std_ulogic;
  begin
    tmp_v := '0';
    for i in a'range loop
      tmp_v := tmp_v xor a(i);
    end loop; -- i
    return tmp_v;
  end function xor_reduce_f;


  -- Function: Bit reversal -----------------------------------------------------------------
  -- -------------------------------------------------------------------------------------------
  function bit_rev_f(input : std_ulogic_vector) return std_ulogic_vector is
    variable output_v : std_ulogic_vector(input'range);
  begin
    for i in 0 to input'length-1 loop
      output_v(input'length-i-1) := input(i);
    end loop; -- i
    return output_v;
  end function bit_rev_f;

  -- Function: Test if input number is a power of two ---------------------------------------
  -- -------------------------------------------------------------------------------------------
  function is_power_of_two_f(input : natural) return boolean is
    variable tmp : unsigned(31 downto 0);
  begin
    if (input = 0) then
      return false;
    elsif (input = 1) then
      return true;
    else
      tmp := to_unsigned(input, 32);
      if ((tmp and (tmp - 1)) = 0) then
        return true;
      else
        return false;
      end if;
    end if;
  end function is_power_of_two_f;

  -- Function: Swap all bytes of a 32-bit word (endianness conversion) ----------------------
  -- -------------------------------------------------------------------------------------------
  function bswap32_f(input : std_ulogic_vector) return std_ulogic_vector is
    variable output_v : std_ulogic_vector(input'range);
  begin
    output_v(07 downto 00) := input(31 downto 24);
    output_v(15 downto 08) := input(23 downto 16);
    output_v(23 downto 16) := input(15 downto 08);
    output_v(31 downto 24) := input(07 downto 00);
    return output_v;
  end function bswap32_f;


end neorv32_package;

