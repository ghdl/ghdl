
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
-- $Id: ch_15_dlx-r.vhd,v 1.3 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

use work.alu_types.all,
  work.reg_file_types.all;

architecture rtl of dlx is

  component alu is
                  port ( s1 : in dlx_word;
                         s2 : in dlx_word;
                         result : out dlx_word;
                         func : in alu_func;
                         zero, negative, overflow : out std_logic );
  end component alu;

  component reg_file is
                       port ( a1 : in reg_file_addr;
                              q1 : out dlx_word;
                              a2 : in reg_file_addr;
                              q2 : out dlx_word;
                              a3 : in reg_file_addr;
                              d3 : in dlx_word;
                              write_en : in std_logic );
  end component reg_file;

  component latch is
                    port ( d : in dlx_word;
                           q : out dlx_word;
                           latch_en : in std_logic );
  end component latch;

  component ir_extender is
                          port ( d : in dlx_word;
                                 q : out dlx_word;
                                 immed_size_26 : in std_logic;
                                 immed_unsigned : in std_logic;
                                 immed_en : in std_logic );
  end component ir_extender;

  component reg_multiple_out is
                               generic ( num_outputs : positive );
                             port ( d : in dlx_word;
                                    q : out dlx_word_array(1 to num_outputs);
                                    latch_en : in std_logic;
                                    out_en : in std_logic_vector(1 to num_outputs) );
  end component reg_multiple_out;

  component reg_multiple_plus_one_out is
                                        generic ( num_outputs : positive );
                                      port ( d : in dlx_word;
                                             q0 : out dlx_word;
                                             q : out dlx_word_array(1 to num_outputs);
                                             latch_en : in std_logic;
                                             out_en : in std_logic_vector(1 to num_outputs) );
  end component reg_multiple_plus_one_out;

  component reg_multiple_plus_one_out_reset is
                                              generic ( num_outputs : positive );
                                            port ( d : in dlx_word;
                                                   q0 : out dlx_word;
                                                   q : out dlx_word_array(1 to num_outputs);
                                                   latch_en : in std_logic;
                                                   out_en : in std_logic_vector(1 to num_outputs);
                                                   reset : in std_logic );
  end component reg_multiple_plus_one_out_reset;

  component mux2 is
                   port ( i0, i1 : in dlx_word;
                          y : out dlx_word;
                          sel : in std_logic);
  end component mux2;

  component controller is
                         port ( phi1, phi2 : in std_logic;
                                reset : in std_logic;
                                halt : out std_logic;
                                width : out dlx_mem_width;
                                write_enable : out std_logic;
                                mem_enable : out std_logic;
                                ifetch : out std_logic;
                                ready : in std_logic;
                                alu_in_latch_en : out std_logic;
                                alu_function : out alu_func;
                                alu_zero, alu_negative, alu_overflow : in std_logic;
                                reg_s1_addr, reg_s2_addr, reg_dest_addr : out reg_file_addr;
                                reg_write : out std_logic;
                                c_latch_en : out std_logic;
                                a_latch_en, a_out_en : out std_logic;
                                b_latch_en, b_out_en : out std_logic;
                                temp_latch_en, temp_out_en1, temp_out_en2 : out std_logic;
                                iar_latch_en, iar_out_en1, iar_out_en2 : out std_logic;
                                pc_latch_en, pc_out_en1, pc_out_en2 : out std_logic;
                                mar_latch_en, mar_out_en1, mar_out_en2 : out std_logic;
                                mem_addr_mux_sel : out std_logic;
                                mdr_latch_en, mdr_out_en1, mdr_out_en2, mdr_out_en3 : out std_logic;
                                mdr_mux_sel : out std_logic;
                                ir_latch_en : out std_logic;
                                ir_immed1_size_26, ir_immed2_size_26 : out std_logic;
                                ir_immed1_unsigned, ir_immed2_unsigned : out std_logic;
                                ir_immed1_en, ir_immed2_en : out std_logic;
                                current_instruction : in dlx_word;
                                mem_addr : std_logic_vector(1 downto 0);
                                const1, const2 : out dlx_word );
  end component controller;


  signal s1_bus, s2_bus : dlx_word;
  signal dest_bus : dlx_word;
  signal alu_in1, alu_in2 : dlx_word;
  signal reg_file_out1, reg_file_out2, reg_file_in : dlx_word;
  signal mdr_in : dlx_word;
  signal current_instruction : dlx_word;
  signal pc_to_mem : dlx_address;
  signal mar_to_mem : dlx_address;

  signal alu_in_latch_en : std_logic;
  signal alu_function : alu_func;
  signal alu_zero, alu_negative, alu_overflow : std_logic;
  signal reg_s1_addr, reg_s2_addr, reg_dest_addr : reg_file_addr;
  signal reg_write : std_logic;
  signal a_out_en, a_latch_en : std_logic;
  signal b_out_en, b_latch_en : std_logic;
  signal c_latch_en : std_logic;
  signal temp_out_en1, temp_out_en2, temp_latch_en : std_logic;
  signal iar_out_en1, iar_out_en2, iar_latch_en : std_logic;
  signal pc_out_en1, pc_out_en2, pc_latch_en : std_logic;
  signal mar_out_en1, mar_out_en2, mar_latch_en : std_logic;
  signal mem_addr_mux_sel : std_logic;
  signal mdr_out_en1, mdr_out_en2, mdr_out_en3, mdr_latch_en : std_logic;
  signal mdr_mux_sel : std_logic;
  signal ir_latch_en : std_logic;
  signal ir_immed1_size_26, ir_immed2_size_26 : std_logic;
  signal ir_immed1_unsigned, ir_immed2_unsigned : std_logic;
  signal ir_immed1_en, ir_immed2_en : std_logic;

begin

  alu_s1_reg : component latch
    port map ( d => s1_bus, q => alu_in1, latch_en => alu_in_latch_en );

  alu_s2_reg : component latch
    port map ( d => s2_bus, q => alu_in2, latch_en => alu_in_latch_en );

  the_alu : component alu
    port map ( s1 => alu_in1, s2 => alu_in2, result => dest_bus,
               func => alu_function,
               zero => alu_zero, negative => alu_negative, overflow => alu_overflow );

  the_reg_file : component reg_file
    port map ( a1 => reg_s1_addr, q1 => reg_file_out1,
               a2 => reg_s2_addr, q2 => reg_file_out2,
               a3 => reg_dest_addr, d3 => reg_file_in,
               write_en => reg_write );

  c_reg : component latch
    port map ( d => dest_bus, q => reg_file_in, latch_en => c_latch_en );

  a_reg : component reg_multiple_out
    generic map ( num_outputs => 1 )
    port map ( d => reg_file_out1, q(1) => s1_bus,
               latch_en => a_latch_en, out_en(1) => a_out_en );

  b_reg : component reg_multiple_out
    generic map ( num_outputs => 1 )
    port map ( d => reg_file_out2, q(1) => s2_bus,
               latch_en => b_latch_en, out_en(1) => b_out_en );

  temp_reg : component reg_multiple_out
    generic map ( num_outputs => 2 )
    port map ( d => dest_bus, q(1) => s1_bus, q(2) => s2_bus,
               latch_en => temp_latch_en,
               out_en(1) => temp_out_en1, out_en(2) => temp_out_en2 );

  iar_reg : component reg_multiple_out
    generic map ( num_outputs => 2 )
    port map ( d => dest_bus, q(1) => s1_bus, q(2) => s2_bus,
               latch_en => iar_latch_en,
               out_en(1) => iar_out_en1, out_en(2) => iar_out_en2 );

  pc_reg : component reg_multiple_plus_one_out_reset
    generic map ( num_outputs => 2 )
    port map ( d => dest_bus, q(1) => s1_bus, q(2) => s2_bus, q0 => pc_to_mem,
               latch_en => pc_latch_en,
               out_en(1) => pc_out_en1, out_en(2) => pc_out_en2,
	       reset => reset );

  mar_reg : component reg_multiple_plus_one_out
    generic map ( num_outputs => 2 )
    port map ( d => dest_bus, q(1) => s1_bus, q(2) => s2_bus, q0 => mar_to_mem,
               latch_en => mar_latch_en,
               out_en(1) => mar_out_en1, out_en(2) => mar_out_en2 );

  mem_addr_mux : component mux2
    port map ( i0 => pc_to_mem, i1 => mar_to_mem, y => a,
               sel => mem_addr_mux_sel );

  mdr_reg : component reg_multiple_out
    generic map ( num_outputs => 3 )
    port map ( d => mdr_in, q(1) => s1_bus, q(2) => s2_bus, q(3) => d,
               latch_en => mdr_latch_en,
               out_en(1) => mdr_out_en1, out_en(2) => mdr_out_en2,
	       out_en(3) => mdr_out_en3 );

  mdr_mux : component mux2
    port map ( i0 => dest_bus, i1 => d, y => mdr_in,
               sel => mdr_mux_sel );

  instr_reg : component latch
    port map ( d => d, q => current_instruction,
               latch_en => ir_latch_en );

  ir_extender1 : component ir_extender
    port map ( d => current_instruction, q => s1_bus,
               immed_size_26 => ir_immed1_size_26,
	       immed_unsigned => ir_immed1_unsigned,
               immed_en => ir_immed1_en );

  ir_extender2 : component ir_extender
    port map ( d => current_instruction, q => s2_bus,
               immed_size_26 => ir_immed2_size_26,
	       immed_unsigned => ir_immed2_unsigned,
               immed_en => ir_immed2_en );

  the_controller : component controller
    port map ( phi1 => phi1, phi2 => phi2, reset => reset, halt => halt, 
      	       width => width, write_enable => write_enable, mem_enable => mem_enable,
	       ifetch => ifetch, ready => ready, 
               alu_in_latch_en => alu_in_latch_en, alu_function => alu_function,
	       alu_zero => alu_zero, alu_negative => alu_negative,
	       alu_overflow => alu_overflow,
               reg_s1_addr => reg_s1_addr, reg_s2_addr => reg_s2_addr,
	       reg_dest_addr => reg_dest_addr, reg_write => reg_write, 
               c_latch_en => c_latch_en,
	       a_latch_en => a_latch_en, a_out_en => a_out_en,
	       b_latch_en => b_latch_en, b_out_en =>  b_out_en, 
               temp_latch_en => temp_latch_en,
	       temp_out_en1 => temp_out_en1, temp_out_en2 => temp_out_en2, 
               iar_latch_en => iar_latch_en,
	       iar_out_en1 => iar_out_en1, iar_out_en2 => iar_out_en2, 
               pc_latch_en => pc_latch_en,
	       pc_out_en1 => pc_out_en1, pc_out_en2 => pc_out_en2, 
               mem_addr_mux_sel => mem_addr_mux_sel, mar_latch_en => mar_latch_en,
	       mar_out_en1 => mar_out_en1, mar_out_en2 => mar_out_en2,
               mdr_mux_sel => mdr_mux_sel, mdr_latch_en => mdr_latch_en,
	       mdr_out_en1 => mdr_out_en1, mdr_out_en2 => mdr_out_en2, 
	       mdr_out_en3 => mdr_out_en3, 
               ir_latch_en => ir_latch_en,
	       ir_immed1_size_26 => ir_immed1_size_26,
	       ir_immed2_size_26 => ir_immed2_size_26,
	       ir_immed1_unsigned => ir_immed1_unsigned,
	       ir_immed2_unsigned => ir_immed2_unsigned,
	       ir_immed1_en => ir_immed1_en, ir_immed2_en => ir_immed2_en,  
               current_instruction => current_instruction,
	       mem_addr => mar_to_mem(1 downto 0),
	       const1 => s1_bus, const2 => s2_bus );

end architecture rtl;
