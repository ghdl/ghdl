
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: ch_15_dlxtst-v.vhd,v 1.3 2001-11-03 23:19:37 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

architecture verifier of dlx_test is

  use work.dlx_types.all;

  component clock_gen is
                        port ( phi1, phi2 : out std_logic;
                               reset : out std_logic );
  end component clock_gen;

  component memory is
                     port ( phi1, phi2 : in std_logic;
                            a : in dlx_address;
                            d : inout dlx_word;
                            width : in dlx_mem_width;
                            write_enable : in std_logic;
                            burst : in std_logic := '0';
                            mem_enable : in std_logic;
                            ready : out std_logic );
  end component memory;

  component dlx is
                  port ( phi1, phi2 : in std_logic;
                         reset : in std_logic;
                         halt : out std_logic;
                         a : out dlx_address;
                         d : inout dlx_word;
                         width : out dlx_mem_width;
                         write_enable : out std_logic;
                         ifetch : out std_logic;
                         mem_enable : out std_logic;
                         ready : in std_logic );
  end component dlx;

  signal phi1, phi2, reset : std_logic;

  signal a_behav : dlx_address;
  signal d_behav : dlx_word;
  signal halt_behav : std_logic;
  signal width_behav : dlx_mem_width;
  signal write_enable_behav, mem_enable_behav, ifetch_behav : std_logic;

  signal a_rtl : dlx_address;
  signal d_rtl : dlx_word;
  signal halt_rtl : std_logic;
  signal width_rtl : dlx_mem_width;
  signal write_enable_rtl, mem_enable_rtl, ifetch_rtl : std_logic;

  signal ready_mem : std_logic;

begin

  cg : component clock_gen
    port map ( phi1 => phi1, phi2 => phi2, reset => reset );

  mem : component memory
    port map ( phi1 => phi1, phi2 => phi2,
               a => a_behav, d => d_behav,
               width => width_behav, write_enable => write_enable_behav,
               burst => open,
               mem_enable => mem_enable_behav, ready => ready_mem );

  proc_behav : component dlx
    port map ( phi1 => phi1, phi2 => phi2, reset => reset, halt => halt_behav,
               a => a_behav, d => d_behav,
               width => width_behav, write_enable => write_enable_behav,
               ifetch => ifetch_behav,
               mem_enable => mem_enable_behav, ready => ready_mem );

  proc_rtl : component dlx
    port map ( phi1 => phi1, phi2 => phi2, reset => reset, halt => halt_rtl,
               a => a_rtl, d => d_rtl,
               width => width_rtl, write_enable => write_enable_rtl,
               ifetch => ifetch_rtl,
               mem_enable => mem_enable_rtl, ready => ready_mem );

  verification_section : block is
  begin

    fwd_data_from_mem_to_rtl : 
      d_rtl <= d_behav when mem_enable_rtl = '1'
               and write_enable_rtl = '0' else
               disabled_dlx_word;

    monitor : process

      variable write_command_behav : boolean;
      variable write_command_rtl : boolean;

    begin
      monitor_loop : loop
        -- wait for a command, valid on leading edge of phi2
        wait until rising_edge(phi2)
          and mem_enable_behav = '1' and mem_enable_rtl = '1';
        --
        -- capture the command information
        write_command_behav := write_enable_behav = '1';
        write_command_rtl := write_enable_rtl = '1';
        assert a_behav = a_rtl
          report "addresses differ";
        assert write_enable_behav = write_enable_rtl
          report "write enable states differ";
        assert ifetch_behav = ifetch_rtl
          report "instruction fetch states differ";
        assert width_behav = width_rtl
          report "widths differ";
        if write_command_behav and write_command_rtl then
          assert d_behav = d_rtl
            report "write data differs";
        end if;
        --
        -- wait for the response from memory
        ready_loop : loop 
          wait until falling_edge(phi2);
          exit monitor_loop when reset = '1';
          exit ready_loop when ready_mem = '1';
        end loop ready_loop;
      end loop monitor_loop;
      --
      -- get here when reset is asserted
      wait until reset = '0';
      --
      -- process monitor now starts again from beginning
    end process monitor;

  end block verification_section;

end architecture verifier;
