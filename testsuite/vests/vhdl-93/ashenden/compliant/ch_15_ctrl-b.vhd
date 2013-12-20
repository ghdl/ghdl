
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
-- $Id: ch_15_ctrl-b.vhd,v 1.3 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library bv_utilities;
use bv_utilities.bv_arithmetic.all;

library work;
use work.dlx_instr.all;

architecture behavior of controller is

begin -- behavior

  sequencer : process is

                        variable current_instruction_bv : dlx_bv_word;

                      alias IR_opcode : dlx_opcode is current_instruction_bv(0 to 5);
                      alias IR_sp_func : dlx_sp_func is current_instruction_bv(26 to 31);
                      alias IR_fp_func : dlx_fp_func is current_instruction_bv(27 to 31);

                      alias IR_rs1 : reg_file_addr is current_instruction(6 to 10);
                      alias IR_rs2 : reg_file_addr is current_instruction(11 to 15);
                      alias IR_Itype_rd : reg_file_addr is current_instruction(11 to 15);
                      alias IR_Rtype_rd : reg_file_addr is current_instruction(16 to 20);

                      variable result_of_set_is_1, branch_taken : boolean;

                      variable disassembled_instr : string(1 to 40);
                      variable disassembled_instr_len : positive;

                      variable instr_count : natural := 0;

                      procedure bus_instruction_fetch is
                      begin
                        -- use PC as address
                        mem_addr_mux_sel <= '0' after Tpd_clk_ctrl;
                        -- set up memory control signals
                        width <= dlx_mem_width_word after Tpd_clk_ctrl;
                        ifetch <= '1' after Tpd_clk_ctrl;
                        write_enable <= '0' after Tpd_clk_ctrl;
                        mem_enable <= '1' after Tpd_clk_ctrl;
                        -- wait until phi2, then enable IR input
                        wait until rising_edge(phi2);
                        ir_latch_en <= '1' after Tpd_clk_ctrl;
                        -- wait until memory is ready at end of phi2
                        loop 
                          wait until falling_edge(phi2);
                          if To_bit(reset) = '1' then
                            return;
                          end if;
                          exit when To_bit(ready) = '1';
                        end loop;
                        -- disable IR input and memory control signals
                        ir_latch_en <= '0' after Tpd_clk_ctrl;
                        mem_enable <= '0' after Tpd_clk_ctrl;
                      end procedure bus_instruction_fetch;

                      procedure bus_data_read ( read_width : in dlx_mem_width ) is
                      begin
                        -- use MAR as address
                        mem_addr_mux_sel <= '1' after Tpd_clk_ctrl;
                        -- set up memory control signals
                        width <= read_width after Tpd_clk_ctrl;
                        ifetch <= '0' after Tpd_clk_ctrl;
                        write_enable <= '0' after Tpd_clk_ctrl;
                        mem_enable <= '1' after Tpd_clk_ctrl;
                        -- wait until phi2, then enable MDR input
                        wait until rising_edge(phi2);
                        mdr_mux_sel <= '1' after Tpd_clk_ctrl;
                        mdr_latch_en <= '1' after Tpd_clk_ctrl;
                        -- wait until memory is ready at end of phi2
                        loop 
                          wait until falling_edge(phi2);
                          if To_bit(reset) = '1' then
                            return;
                          end if;
                          exit when To_bit(ready) = '1';
                        end loop;
                        -- disable MDR input and memory control signals
                        mdr_latch_en <= '0' after Tpd_clk_ctrl;
                        mem_enable <= '0' after Tpd_clk_ctrl;
                      end procedure bus_data_read;

                      procedure bus_data_write ( write_width : in dlx_mem_width ) is
                      begin
                        -- use MAR as address
                        mem_addr_mux_sel <= '1' after Tpd_clk_ctrl;
                        -- enable MDR output
                        mdr_out_en3 <= '1' after Tpd_clk_ctrl;
                        -- set up memory control signals
                        width <= write_width after Tpd_clk_ctrl;
                        ifetch <= '0' after Tpd_clk_ctrl;
                        write_enable <= '1' after Tpd_clk_ctrl;
                        mem_enable <= '1' after Tpd_clk_ctrl;
                        -- wait until memory is ready at end of phi2
                        loop 
                          wait until falling_edge(phi2);
                          if To_bit(reset) = '1' then
                            return;
                          end if;
                          exit when To_bit(ready) = '1';
                        end loop;
                        -- disable MDR output and memory control signals
                        write_enable <= '0' after Tpd_clk_ctrl;
                        mem_enable <= '0' after Tpd_clk_ctrl;
                        mdr_out_en3 <= '0' after Tpd_clk_ctrl;
                      end procedure bus_data_write;

                      procedure do_set_result is
                      begin
                        wait until rising_edge(phi1);
                        if result_of_set_is_1 then
                          const2 <= X"0000_0001" after Tpd_clk_const;
                        else
                          const2 <= X"0000_0000" after Tpd_clk_const;
                        end if;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_pass_s2 after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        const2 <= disabled_dlx_word after Tpd_clk_const;

                        wait until rising_edge(phi2);
                        c_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        c_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_set_result;    

                      procedure do_EX_set_unsigned ( immed : boolean ) is
                      begin
                        wait until rising_edge(phi1);
                        a_out_en <= '1' after Tpd_clk_ctrl;
                        if immed then
                          ir_immed2_size_26 <= '0' after Tpd_clk_ctrl;
                          ir_immed2_unsigned <= '1' after Tpd_clk_ctrl;
                          ir_immed2_en <= '1' after Tpd_clk_ctrl;
                        else
                          b_out_en <= '1' after Tpd_clk_ctrl;
                        end if;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_subu after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        a_out_en <= '0' after Tpd_clk_ctrl;
                        if immed then
                          ir_immed2_en <= '0' after Tpd_clk_ctrl;
                        else
                          b_out_en <= '0' after Tpd_clk_ctrl;
                        end if;

                        wait until falling_edge(phi2);
                        if immed then
                          case IR_opcode is
                            when op_sequi => 
                              result_of_set_is_1 := To_bit(alu_zero) = '1';
                            when op_sneui =>
                              result_of_set_is_1 := To_bit(alu_zero) /= '1';
                            when op_sltui =>
                              result_of_set_is_1 := To_bit(alu_overflow) = '1';
                            when op_sgtui => 
                              result_of_set_is_1 := To_bit(alu_overflow) /= '1' and To_bit(alu_zero) /= '1';
                            when op_sleui => 
                              result_of_set_is_1 := To_bit(alu_overflow) = '1' or To_bit(alu_zero) = '1';
                            when op_sgeui => 
                              result_of_set_is_1 := To_bit(alu_overflow) /= '1';
                            when others =>
                              null;
                          end case;
                        else
                          case IR_sp_func is
                            when sp_func_sequ => 
                              result_of_set_is_1 := To_bit(alu_zero) = '1';
                            when sp_func_sneu =>
                              result_of_set_is_1 := To_bit(alu_zero) /= '1';
                            when sp_func_sltu =>
                              result_of_set_is_1 := To_bit(alu_overflow) = '1';
                            when sp_func_sgtu =>
                              result_of_set_is_1 := To_bit(alu_overflow) /= '1' and To_bit(alu_zero) /= '1';
                            when sp_func_sleu => 
                              result_of_set_is_1 := To_bit(alu_overflow) = '1' or To_bit(alu_zero) = '1';
                            when sp_func_sgeu => 
                              result_of_set_is_1 := To_bit(alu_overflow) /= '1';
                            when others =>
                              null;
                          end case;
                        end if;

                        do_set_result;
                      end procedure do_EX_set_unsigned;

                      procedure do_EX_set_signed ( immed : boolean ) is
                      begin
                        wait until rising_edge(phi1);
                        a_out_en <= '1' after Tpd_clk_ctrl;
                        if immed then
                          ir_immed2_size_26 <= '0' after Tpd_clk_ctrl;
                          ir_immed2_unsigned <= '0' after Tpd_clk_ctrl;
                          ir_immed2_en <= '1' after Tpd_clk_ctrl;
                        else
                          b_out_en <= '1' after Tpd_clk_ctrl;
                        end if;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_sub after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        a_out_en <= '0' after Tpd_clk_ctrl;
                        if immed then
                          ir_immed2_en <= '0' after Tpd_clk_ctrl;
                        else
                          b_out_en <= '0' after Tpd_clk_ctrl;
                        end if;

                        wait until falling_edge(phi2);
                        if immed then
                          case IR_opcode is
                            when op_seqi => 
                              result_of_set_is_1 := To_bit(alu_zero) = '1';
                            when op_snei =>
                              result_of_set_is_1 := To_bit(alu_zero) /= '1';
                            when op_slti =>
                              result_of_set_is_1 := To_bit(alu_negative) = '1';
                            when op_sgti => 
                              result_of_set_is_1 := To_bit(alu_negative) /= '1' and To_bit(alu_zero) /= '1';
                            when op_slei => 
                              result_of_set_is_1 := To_bit(alu_negative) = '1' or To_bit(alu_zero) = '1';
                            when op_sgei => 
                              result_of_set_is_1 := To_bit(alu_negative) /= '1';
                            when others =>
                              null;
                          end case;
                        else
                          case IR_sp_func is
                            when sp_func_seq => 
                              result_of_set_is_1 := To_bit(alu_zero) = '1';
                            when sp_func_sne =>
                              result_of_set_is_1 := To_bit(alu_zero) /= '1';
                            when sp_func_slt =>
                              result_of_set_is_1 := To_bit(alu_negative) = '1';
                            when sp_func_sgt => 
                              result_of_set_is_1 := To_bit(alu_negative) /= '1' and To_bit(alu_zero) /= '1';
                            when sp_func_sle => 
                              result_of_set_is_1 := To_bit(alu_negative) = '1' or To_bit(alu_zero) = '1';
                            when sp_func_sge => 
                              result_of_set_is_1 := To_bit(alu_negative) /= '1';
                            when others =>
                              null;
                          end case;
                        end if;

                        do_set_result;
                      end procedure do_EX_set_signed;

                      procedure do_EX_arith_logic is
                      begin
                        wait until rising_edge(phi1);
                        a_out_en <= '1' after Tpd_clk_ctrl;
                        b_out_en <= '1' after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        case IR_sp_func is
                          when sp_func_add =>
                            alu_function <= alu_add after Tpd_clk_ctrl;
                          when sp_func_addu =>
                            alu_function <= alu_addu after Tpd_clk_ctrl;
                          when sp_func_sub =>
                            alu_function <= alu_sub after Tpd_clk_ctrl;
                          when sp_func_subu =>
                            alu_function <= alu_subu after Tpd_clk_ctrl;
                          when sp_func_and =>
                            alu_function <= alu_and after Tpd_clk_ctrl;
                          when sp_func_or =>
                            alu_function <= alu_or after Tpd_clk_ctrl;
                          when sp_func_xor =>
                            alu_function <= alu_xor after Tpd_clk_ctrl;
                          when sp_func_sll =>
                            alu_function <= alu_sll after Tpd_clk_ctrl;
                          when sp_func_srl =>
                            alu_function <= alu_srl after Tpd_clk_ctrl;
                          when sp_func_sra =>
                            alu_function <= alu_sra after Tpd_clk_ctrl;
                          when others =>
                            null;
                        end case;                  --  IR_sp_func

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        a_out_en <= '0' after Tpd_clk_ctrl;
                        b_out_en <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        c_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        c_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_EX_arith_logic;

                      procedure do_EX_arith_logic_immed is
                      begin
                        wait until rising_edge(phi1);
                        a_out_en <= '1' after Tpd_clk_ctrl;
                        ir_immed2_size_26 <= '0' after Tpd_clk_ctrl;
                        if IR_opcode = op_addi or IR_opcode = op_subi then
                          ir_immed2_unsigned <= '0' after Tpd_clk_ctrl;
                        else
                          ir_immed2_unsigned <= '1' after Tpd_clk_ctrl;
                        end if;
                        ir_immed2_en <= '1' after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        case IR_opcode is
                          when op_addi =>
                            alu_function <= alu_add after Tpd_clk_ctrl;
                          when op_subi =>
                            alu_function <= alu_sub after Tpd_clk_ctrl;
                          when op_addui =>
                            alu_function <= alu_addu after Tpd_clk_ctrl;
                          when op_subui =>
                            alu_function <= alu_subu after Tpd_clk_ctrl;
                          when op_andi =>
                            alu_function <= alu_and after Tpd_clk_ctrl;
                          when op_ori =>
                            alu_function <= alu_or after Tpd_clk_ctrl;
                          when op_xori =>
                            alu_function <= alu_xor after Tpd_clk_ctrl;
                          when op_slli =>
                            alu_function <= alu_sll after Tpd_clk_ctrl;
                          when op_srli =>
                            alu_function <= alu_srl after Tpd_clk_ctrl;
                          when op_srai =>
                            alu_function <= alu_sra after Tpd_clk_ctrl;
                          when others =>
                            null;
                        end case;                      --  IR_opcode

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        a_out_en <= '0' after Tpd_clk_ctrl;
                        ir_immed2_en <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        c_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        c_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_EX_arith_logic_immed;

                      procedure do_EX_link is
                      begin
                        wait until rising_edge(phi1);
                        pc_out_en1 <= '1' after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_pass_s1 after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        pc_out_en1 <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        c_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        c_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_EX_link;

                      procedure do_EX_lhi is
                      begin
                        wait until rising_edge(phi1);
                        ir_immed1_size_26 <= '0' after Tpd_clk_ctrl;
                        ir_immed1_unsigned <= '1' after Tpd_clk_ctrl;
                        ir_immed1_en <= '1' after Tpd_clk_ctrl;
                        const2 <= X"0000_0010" after Tpd_clk_const;         -- shift by 16 bits
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_sll after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        ir_immed1_en <= '0' after Tpd_clk_ctrl;
                        const2 <= disabled_dlx_word after Tpd_clk_const;

                        wait until rising_edge(phi2);
                        c_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        c_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_EX_lhi;    

                      procedure do_EX_branch is
                      begin
                        wait until rising_edge(phi1);
                        a_out_en <= '1' after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_pass_s1 after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        a_out_en <= '0' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        if IR_opcode = op_beqz then
                          branch_taken := To_bit(alu_zero) = '1';
                        else
                          branch_taken := To_bit(alu_zero) /= '1';
                        end if;
                      end procedure do_EX_branch;

                      procedure do_EX_load_store is
                      begin
                        wait until rising_edge(phi1);
                        a_out_en <= '1' after Tpd_clk_ctrl;
                        ir_immed2_size_26 <= '0' after Tpd_clk_ctrl;
                        ir_immed2_unsigned <= '0' after Tpd_clk_ctrl;
                        ir_immed2_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_add after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        a_out_en <= '0' after Tpd_clk_ctrl;
                        ir_immed2_en <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        mar_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        mar_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_EX_load_store;

                      procedure do_MEM_jump is
                      begin
                        wait until rising_edge(phi1);
                        pc_out_en1 <= '1' after Tpd_clk_ctrl;
                        ir_immed2_size_26 <= '1' after Tpd_clk_ctrl;
                        ir_immed2_unsigned <= '0' after Tpd_clk_ctrl;
                        ir_immed2_en <= '1' after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_add after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        pc_out_en1 <= '0' after Tpd_clk_ctrl;
                        ir_immed2_en <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        pc_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        pc_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_MEM_jump;

                      procedure do_MEM_jump_reg is
                      begin
                        wait until rising_edge(phi1);
                        a_out_en <= '1' after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_pass_s1 after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        a_out_en <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        pc_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        pc_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_MEM_jump_reg;

                      procedure do_MEM_branch is
                      begin
                        wait until rising_edge(phi1);
                        pc_out_en1 <= '1' after Tpd_clk_ctrl;
                        ir_immed2_size_26 <= '0' after Tpd_clk_ctrl;
                        ir_immed2_unsigned <= '0' after Tpd_clk_ctrl;
                        ir_immed2_en <= '1' after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_add after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;
                        pc_out_en1 <= '0' after Tpd_clk_ctrl;
                        ir_immed2_en <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        pc_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        pc_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_MEM_branch;

                      procedure do_MEM_load is
                        subtype ls_2_addr_bits is bit_vector(1 downto 0);
                      begin
                        wait until rising_edge(phi1);
                        if IR_opcode = op_lb or IR_opcode = op_lbu then
                          bus_data_read(dlx_mem_width_byte);
                        elsif IR_opcode = op_lh or IR_opcode = op_lhu then
                          bus_data_read(dlx_mem_width_halfword);
                        else
                          bus_data_read(dlx_mem_width_word);
                        end if;
                        if To_bit(reset) = '1' then
                          return;
                        end if;

                        if ( (IR_opcode = op_lb or IR_opcode = op_lbu) and To_bitvector(mem_addr) /= "00" )
                          or ( (IR_opcode = op_lh or IR_opcode = op_lhu) and To_bit(mem_addr(1)) /= '0' ) then
                          -- first step of extension: left-justify byte or halfword -> mdr
                          wait until rising_edge(phi1);
                          mdr_out_en1 <= '1' after Tpd_clk_ctrl;
                          if IR_opcode = op_lb or IR_opcode = op_lbu then
                            case ls_2_addr_bits'(To_bitvector(mem_addr)) is
                              when "00" => 
                                null;
                              when "01" => 
                                const2 <= X"0000_0008" after Tpd_clk_const;
                              when "10" => 
                                const2 <= X"0000_0010" after Tpd_clk_const;
                              when "11" => 
                                const2 <= X"0000_0018" after Tpd_clk_const;
                            end case;
                          else
                            const2 <= X"0000_0010" after Tpd_clk_const;
                          end if;
                          alu_function <= alu_sll after Tpd_clk_ctrl;
                          alu_in_latch_en <= '1' after Tpd_clk_ctrl;

                          wait until falling_edge(phi1);
                          mdr_out_en1 <= '0' after Tpd_clk_ctrl;
                          const2 <= disabled_dlx_word after Tpd_clk_const;
                          alu_in_latch_en <= '0' after Tpd_clk_ctrl;

                          wait until rising_edge(phi2);
                          mdr_mux_sel <= '0' after Tpd_clk_ctrl;
                          mdr_latch_en <= '1' after Tpd_clk_ctrl;

                          wait until falling_edge(phi2);
                          mdr_latch_en <= '0' after Tpd_clk_ctrl;
                        end if;

                        wait until rising_edge(phi1);
                        mdr_out_en1 <= '1' after Tpd_clk_ctrl;
                        if IR_opcode = op_lb or IR_opcode = op_lbu then
                          const2 <= X"0000_0018" after Tpd_clk_const;
                        elsif IR_opcode = op_lh or IR_opcode = op_lhu then
                          const2 <= X"0000_0010" after Tpd_clk_const;
                        else
                          const2 <= X"0000_0000" after Tpd_clk_const;
                        end if;
                        if IR_opcode = op_lbu or IR_opcode = op_lhu then
                          alu_function <= alu_srl after Tpd_clk_ctrl;
                        else
                          alu_function <= alu_sra after Tpd_clk_ctrl;
                        end if;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        mdr_out_en1 <= '0' after Tpd_clk_ctrl;
                        const2 <= disabled_dlx_word after Tpd_clk_const;
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        c_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        c_latch_en <= '0' after Tpd_clk_ctrl;
                      end procedure do_MEM_load;    

                      procedure do_MEM_store is
                        subtype ls_2_addr_bits is bit_vector(1 downto 0);
                      begin
                        wait until rising_edge(phi1);
                        b_out_en <= '1' after Tpd_clk_ctrl;
                        alu_function <= alu_pass_s2 after Tpd_clk_ctrl;
                        alu_in_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi1);
                        b_out_en <= '0' after Tpd_clk_ctrl;
                        alu_in_latch_en <= '0' after Tpd_clk_ctrl;

                        wait until rising_edge(phi2);
                        mdr_mux_sel <= '0' after Tpd_clk_ctrl;
                        mdr_latch_en <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        mdr_latch_en <= '0' after Tpd_clk_ctrl;

                        if ( IR_opcode = op_sb and To_bitvector(mem_addr) /= "11" )
                          or ( IR_opcode = op_sh and To_bit(mem_addr(1)) /= '1' ) then
                          -- align byte or halfword -> mdr
                          wait until rising_edge(phi1);
                          mdr_out_en1 <= '1' after Tpd_clk_ctrl;
                          if IR_opcode = op_sb then
                            case ls_2_addr_bits'(To_bitvector(mem_addr)) is
                              when "00" => 
                                const2 <= X"0000_0018" after Tpd_clk_const;
                              when "01" => 
                                const2 <= X"0000_0010" after Tpd_clk_const;
                              when "10" => 
                                const2 <= X"0000_0008" after Tpd_clk_const;
                              when "11" => 
                                null;
                            end case;
                          else
                            const2 <= X"0000_0010" after Tpd_clk_const;
                          end if;
                          alu_function <= alu_sll after Tpd_clk_ctrl;
                          alu_in_latch_en <= '1' after Tpd_clk_ctrl;

                          wait until falling_edge(phi1);
                          mdr_out_en1 <= '0' after Tpd_clk_ctrl;
                          const2 <= disabled_dlx_word after Tpd_clk_const;
                          alu_in_latch_en <= '0' after Tpd_clk_ctrl;

                          wait until rising_edge(phi2);
                          mdr_mux_sel <= '0' after Tpd_clk_ctrl;
                          mdr_latch_en <= '1' after Tpd_clk_ctrl;

                          wait until falling_edge(phi2);
                          mdr_latch_en <= '0' after Tpd_clk_ctrl;
                        end if;

                        wait until rising_edge(phi1);
                        if IR_opcode = op_sb then
                          bus_data_write(dlx_mem_width_byte);
                        elsif IR_opcode = op_sh then
                          bus_data_write(dlx_mem_width_halfword);
                        else
                          bus_data_write(dlx_mem_width_word);
                        end if;
                      end procedure do_MEM_store;

                      procedure do_WB ( Rd : reg_file_addr ) is
                      begin
                        wait until rising_edge(phi1);
                        reg_dest_addr <= Rd after Tpd_clk_ctrl;
                        reg_write <= '1' after Tpd_clk_ctrl;

                        wait until falling_edge(phi2);
                        reg_write <= '0' after Tpd_clk_ctrl;
                      end procedure do_WB;

                      procedure execute_op_special is
                      begin
                        case IR_sp_func is
                          when sp_func_nop =>
                            null;
                          when sp_func_add | sp_func_addu | sp_func_sub | sp_func_subu
                            | sp_func_sll | sp_func_srl | sp_func_sra
                            | sp_func_and | sp_func_or | sp_func_xor =>
                            do_EX_arith_logic;
                            do_WB(IR_Rtype_rd);
                          when sp_func_sequ | sp_func_sneu | sp_func_sltu
                            | sp_func_sgtu | sp_func_sleu | sp_func_sgeu => 
                            do_EX_set_unsigned(immed => false);
                            do_WB(IR_Rtype_rd);
                          when sp_func_seq | sp_func_sne | sp_func_slt
                            | sp_func_sgt | sp_func_sle | sp_func_sge => 
                            do_EX_set_signed(immed => false);
                            do_WB(IR_Rtype_rd);
                          when sp_func_movi2s | sp_func_movs2i
                            | sp_func_movf | sp_func_movd
                            | sp_func_movfp2i | sp_func_movi2fp  => 
                            report sp_func_names(bv_to_natural(IR_sp_func))
                              & " instruction not implemented" severity warning;
                          when others =>
                            report "undefined special instruction function" severity error;
                        end case;  
                      end procedure execute_op_special;

                      procedure execute_op_fparith is
                      begin
                        case IR_fp_func is
                          when fp_func_mult | fp_func_multu | fp_func_div | fp_func_divu
                            | fp_func_addf | fp_func_subf | fp_func_multf | fp_func_divf
                            | fp_func_addd | fp_func_subd | fp_func_multd | fp_func_divd
                            | fp_func_cvtf2d | fp_func_cvtf2i | fp_func_cvtd2f
                            | fp_func_cvtd2i | fp_func_cvti2f | fp_func_cvti2d
                            | fp_func_eqf | fp_func_nef | fp_func_ltf | fp_func_gtf
                            | fp_func_lef | fp_func_gef | fp_func_eqd | fp_func_ned
                            | fp_func_ltd | fp_func_gtd | fp_func_led | fp_func_ged => 
                            report fp_func_names(bv_to_natural(IR_fp_func))
                              & " instruction not implemented" severity warning;
                          when others =>
                            report "undefined floating point instruction function" severity error;
                        end case;
                      end procedure execute_op_fparith;

  begin -- sequencer

    ----------------------------------------------------------------
    -- initialize all control signals
    ----------------------------------------------------------------
    if debug > none then
      report "initializing";
    end if;

    halt <= '0' after Tpd_clk_ctrl;
    width <= dlx_mem_width_word after Tpd_clk_ctrl;
    write_enable <= '0' after Tpd_clk_ctrl;
    mem_enable <= '0' after Tpd_clk_ctrl;
    ifetch <= '0' after Tpd_clk_ctrl;
    alu_in_latch_en <= '0' after Tpd_clk_ctrl;
    alu_function <= alu_add after Tpd_clk_ctrl;
    reg_s1_addr <= B"00000" after Tpd_clk_ctrl;
    reg_s2_addr <= B"00000" after Tpd_clk_ctrl;
    reg_dest_addr <= B"00000" after Tpd_clk_ctrl;
    reg_write <= '0' after Tpd_clk_ctrl;
    c_latch_en <= '0' after Tpd_clk_ctrl;
    a_latch_en <= '0' after Tpd_clk_ctrl;
    a_out_en <= '0' after Tpd_clk_ctrl;
    b_latch_en <= '0' after Tpd_clk_ctrl;
    b_out_en <= '0' after Tpd_clk_ctrl;
    temp_latch_en <= '0' after Tpd_clk_ctrl;
    temp_out_en1 <= '0' after Tpd_clk_ctrl;
    temp_out_en2 <= '0' after Tpd_clk_ctrl;
    iar_latch_en <= '0' after Tpd_clk_ctrl;
    iar_out_en1 <= '0' after Tpd_clk_ctrl;
    iar_out_en2 <= '0' after Tpd_clk_ctrl;
    pc_latch_en <= '0' after Tpd_clk_ctrl;
    pc_out_en1 <= '0' after Tpd_clk_ctrl;
    pc_out_en2 <= '0' after Tpd_clk_ctrl;
    mar_latch_en <= '0' after Tpd_clk_ctrl;
    mar_out_en1 <= '0' after Tpd_clk_ctrl;
    mar_out_en2 <= '0' after Tpd_clk_ctrl;
    mem_addr_mux_sel <= '0' after Tpd_clk_ctrl;
    mdr_latch_en <= '0' after Tpd_clk_ctrl;
    mdr_out_en1 <= '0' after Tpd_clk_ctrl;
    mdr_out_en2 <= '0' after Tpd_clk_ctrl;
    mdr_out_en3 <= '0' after Tpd_clk_ctrl;
    mdr_mux_sel <= '0' after Tpd_clk_ctrl;
    ir_latch_en <= '0' after Tpd_clk_ctrl;
    ir_immed1_size_26 <= '0' after Tpd_clk_ctrl;
    ir_immed2_size_26 <= '0' after Tpd_clk_ctrl;
    ir_immed2_unsigned <= '0' after Tpd_clk_ctrl;
    ir_immed2_unsigned <= '0' after Tpd_clk_ctrl;
    ir_immed1_en <= '0' after Tpd_clk_ctrl;
    ir_immed2_en <= '0' after Tpd_clk_ctrl;
    const1 <= disabled_dlx_word after Tpd_clk_const;
    const2 <= disabled_dlx_word after Tpd_clk_const;

    instr_count := 0;

    wait on phi2 until falling_edge(phi2) and To_bit(reset) = '0';

    ----------------------------------------------------------------
    -- control loop
    ----------------------------------------------------------------
    loop
      exit when To_bit(reset) = '1';

      ----------------------------------------------------------------
      -- fetch next instruction (IF)
      ----------------------------------------------------------------
      wait until rising_edge(phi1);

      instr_count := instr_count + 1;
      if debug = msg_every_100_instructions and instr_count mod 100 = 0 then
        report "instruction count = " & natural'image(instr_count);
      end if;

      if debug >= msg_each_instruction then
        report "fetching instruction";
      end if;

      bus_instruction_fetch;
      exit when To_bit(reset) = '1';
      current_instruction_bv := To_bitvector(current_instruction);

      if debug >= trace_each_instruction then
        disassemble(current_instruction_bv, disassembled_instr, disassembled_instr_len);
        report disassembled_instr(1 to disassembled_instr_len);
      end if;

      ----------------------------------------------------------------
      -- instruction decode, source register read and PC increment (ID)
      ----------------------------------------------------------------
      wait until rising_edge(phi1);

      if debug = trace_each_step then
        report "decode, source register read and PC increment";
      end if;

      reg_s1_addr <= IR_rs1 after Tpd_clk_ctrl;
      reg_s2_addr <= IR_rs2 after Tpd_clk_ctrl;
      a_latch_en <= '1' after Tpd_clk_ctrl;
      b_latch_en <= '1' after Tpd_clk_ctrl;

      pc_out_en1 <= '1' after Tpd_clk_ctrl;
      const2 <= X"0000_0004" after Tpd_clk_const;
      alu_in_latch_en <= '1' after Tpd_clk_ctrl;
      alu_function <= alu_addu after Tpd_clk_ctrl;

      wait until falling_edge(phi1);
      a_latch_en <= '0' after Tpd_clk_ctrl;
      b_latch_en <= '0' after Tpd_clk_ctrl;
      alu_in_latch_en <= '0' after Tpd_clk_ctrl;
      pc_out_en1 <= '0' after Tpd_clk_ctrl;
      const2 <= disabled_dlx_word after Tpd_clk_const;

      wait until rising_edge(phi2);
      pc_latch_en <= '1' after Tpd_clk_ctrl;

      wait until falling_edge(phi2);
      pc_latch_en <= '0' after Tpd_clk_ctrl;

      ----------------------------------------------------------------
      -- execute instruction, (EX, MEM, WB)
      ----------------------------------------------------------------
      if debug = trace_each_step then
        report "execute";
      end if;

      case IR_opcode is
        when op_special =>
          execute_op_special;
        when op_fparith => 
          execute_op_fparith;
        when op_j =>
          do_MEM_jump;
        when op_jal =>
          do_EX_link;
          do_MEM_jump;
          do_WB(To_X01(natural_to_bv(link_reg, 5)));
        when op_jr =>
          do_MEM_jump_reg;
        when op_jalr =>
          do_EX_link;
          do_MEM_jump_reg;
          do_WB(To_X01(natural_to_bv(link_reg, 5)));
        when op_beqz | op_bnez => 
          do_EX_branch;
          if branch_taken then
            do_MEM_branch;
          end if;
        when op_addi | op_subi | op_addui | op_subui 
          | op_slli | op_srli | op_srai
          | op_andi | op_ori | op_xori =>
          do_EX_arith_logic_immed;
          do_WB(IR_Itype_rd);
        when op_lhi =>
          do_EX_lhi;
          do_WB(IR_Itype_rd);
        when op_sequi | op_sneui | op_sltui
          | op_sgtui | op_sleui | op_sgeui => 
          do_EX_set_unsigned(immed => true);
          do_WB(IR_Itype_rd);
        when op_seqi | op_snei | op_slti
          | op_sgti | op_slei | op_sgei => 
          do_EX_set_signed(immed => true);
          do_WB(IR_Itype_rd);
        when op_trap =>
          report "TRAP instruction encountered, execution halted"
            severity note;
          wait until rising_edge(phi1);
          halt <= '1' after Tpd_clk_ctrl;
          wait until reset = '1';
          exit;
        when op_lb | op_lh | op_lw | op_lbu | op_lhu => 
          do_EX_load_store;
          do_MEM_load;
          exit when reset = '1';
          do_WB(IR_Itype_rd);
        when op_sb | op_sh | op_sw =>
          do_EX_load_store;
          do_MEM_store;
          exit when reset = '1';
        when op_rfe | op_bfpt | op_bfpf | op_lf | op_ld | op_sf | op_sd =>
          report opcode_names(bv_to_natural(IR_opcode))
            & " instruction not implemented" severity warning;
        when others =>
          report "undefined instruction" severity error;
      end case;

      -- overflow and divide-by-zero exception handing
      -- (not implemented)

      if debug = trace_each_step then
        report "end of execution";
      end if;

    end loop;
    -- loop is only exited when reset active:
    -- process interpreter starts again from beginning
  end process sequencer;


end architecture behavior;
