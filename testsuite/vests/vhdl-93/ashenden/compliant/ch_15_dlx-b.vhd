
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
-- $Id: ch_15_dlx-b.vhd,v 1.4 2001-11-03 23:19:37 paw Exp $
-- $Revision: 1.4 $
--
-- ---------------------------------------------------------------------

library bv_utilities;
use bv_utilities.bv_arithmetic.all;

library work;
use work.dlx_instr.all;

architecture behavior of dlx is
begin

  interpreter : process
    is

    type reg_array is array (reg_index) of dlx_bv_word;
    variable reg : reg_array;
    variable fp_reg : reg_array;

    variable PC : dlx_bv_word;
    constant PC_incr : dlx_bv_word := X"0000_0004";

    variable IR : dlx_bv_word;
    alias IR_opcode : dlx_opcode is IR(0 to 5);
    alias IR_sp_func : dlx_sp_func is IR(26 to 31);
    alias IR_fp_func : dlx_fp_func is IR(27 to 31);
    alias IR_rs1 : dlx_reg_addr is IR(6 to 10);
    alias IR_rs2 : dlx_reg_addr is IR(11 to 15);
    alias IR_Itype_rd : dlx_reg_addr is IR(11 to 15);
    alias IR_Rtype_rd : dlx_reg_addr is IR(16 to 20);
    alias IR_immed16 : dlx_immed16 is IR(16 to 31);
    alias IR_immed26 : dlx_immed26 is IR(6 to 31);

    variable disassembled_instr : string(1 to 40);
    variable disassembled_instr_len : positive;

    variable rs1, rs2, Itype_rd, Rtype_rd : reg_index;

    variable mem_addr_reg : dlx_bv_address;
    variable mem_data_reg : dlx_bv_word;

    variable overflow : boolean;

    -- lookup table for result of set instructions
    type set_result_table is array (boolean) of dlx_bv_word;
    constant set_if : set_result_table := ( false => X"0000_0000",
                                            true => X"0000_0001" );
    variable instr_count : natural;


    -- local procedures for use within the interpreter


    procedure bus_read ( address : in dlx_bv_address;
                         data_width : in dlx_mem_width;
                         instr_fetch : in std_logic;
                         data : out dlx_bv_word ) is

    begin
      wait until rising_edge(phi1);
      if To_bit(reset) = '1' then
        return;
      end if;
      a <= To_X01(address) after Tpd_clk_out;
      width <= data_width after Tpd_clk_out;
      ifetch <= instr_fetch after Tpd_clk_out;
      mem_enable <= '1' after Tpd_clk_out;
      loop 
        wait until falling_edge(phi2);
        if To_bit(reset) = '1' then
          return;
        end if;
        exit when To_bit(ready) = '1';
      end loop;
      assert not Is_X(d) report "Bus read data contains unknown bits";
      data := To_bitvector(d);
      mem_enable <= '0' after Tpd_clk_out;
    end procedure bus_read;


    procedure bus_write ( address : in dlx_bv_address;
                          data_width : in dlx_mem_width;
                          data : in dlx_bv_word ) is

    begin
      wait until rising_edge(phi1);
      if To_bit(reset) = '1' then
        return;
      end if;
      a <= To_X01(address) after Tpd_clk_out;
      ifetch <= '0' after Tpd_clk_out;
      width <= data_width after Tpd_clk_out;
      d <= To_X01Z(data) after Tpd_clk_out;
      write_enable <= '1' after Tpd_clk_out;
      mem_enable <= '1' after Tpd_clk_out;
      loop 
        wait until falling_edge(phi2);
        if To_bit(reset) = '1' then
          return;
        end if;
        exit when To_bit(ready) = '1';
      end loop;
      d <= disabled_dlx_word after Tpd_clk_out;
      write_enable <= '0' after Tpd_clk_out;
      mem_enable <= '0' after Tpd_clk_out;
    end procedure bus_write;


    procedure execute_op_special is
    begin
      case IR_sp_func is
        when sp_func_nop =>
          null;
        when sp_func_add => 
          bv_add(reg(rs1), reg(rs2), reg(Rtype_rd), overflow);
        when sp_func_addu => 
          bv_addu(reg(rs1), reg(rs2), reg(Rtype_rd), overflow);
        when sp_func_sub => 
          bv_sub(reg(rs1), reg(rs2), reg(Rtype_rd), overflow);
        when sp_func_subu => 
          bv_subu(reg(rs1), reg(rs2), reg(Rtype_rd), overflow);
        when sp_func_sll => 
          reg(Rtype_rd) := reg(rs1) sll bv_to_natural(reg(rs2)(27 to 31));
        when sp_func_srl => 
          reg(Rtype_rd) := reg(rs1) srl bv_to_natural(reg(rs2)(27 to 31));
        when sp_func_sra => 
          reg(Rtype_rd) := reg(rs1) sra bv_to_natural(reg(rs2)(27 to 31));
        when sp_func_and => 
          reg(Rtype_rd) := reg(rs1) and reg(rs2);
        when sp_func_or => 
          reg(Rtype_rd) := reg(rs1) or reg(rs2);
        when sp_func_xor => 
          reg(Rtype_rd) := reg(rs1) xor reg(rs2);
        when sp_func_sequ =>
          reg(Rtype_rd) := set_if( reg(rs1) = reg(rs2) );
        when sp_func_sneu => 
          reg(Rtype_rd) := set_if( reg(rs1) /= reg(rs2) );
        when sp_func_sltu => 
          reg(Rtype_rd) := set_if( reg(rs1) < reg(rs2) );
        when sp_func_sgtu => 
          reg(Rtype_rd) := set_if( reg(rs1) > reg(rs2) );
        when sp_func_sleu => 
          reg(Rtype_rd) := set_if( reg(rs1) <= reg(rs2) );
        when sp_func_sgeu => 
          reg(Rtype_rd) := set_if( reg(rs1) >= reg(rs2) );
        when sp_func_seq => 
          reg(Rtype_rd) := set_if( reg(rs1) = reg(rs2) );
        when sp_func_sne => 
          reg(Rtype_rd) := set_if( reg(rs1) /= reg(rs2) );
        when sp_func_slt => 
          reg(Rtype_rd) := set_if( bv_lt(reg(rs1), reg(rs2)) );
        when sp_func_sgt => 
          reg(Rtype_rd) := set_if( bv_gt(reg(rs1), reg(rs2)) );
        when sp_func_sle => 
          reg(Rtype_rd) := set_if( bv_le(reg(rs1), reg(rs2)) );
        when sp_func_sge => 
          reg(Rtype_rd) := set_if( bv_ge(reg(rs1), reg(rs2)) );
        when sp_func_movi2s | sp_func_movs2i
          | sp_func_movf | sp_func_movd
          | sp_func_movfp2i | sp_func_movi2fp => 
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


    procedure execute_load ( data_width : dlx_mem_width; unsigned : boolean ) is

      variable temp : dlx_bv_word;

      -- type for least-significant two bits of address
      subtype ls_2_addr_bits is bit_vector(1 downto 0);

    begin
      mem_addr_reg := reg(rs1) + bv_sext(IR_immed16, 32);
      bus_read(mem_addr_reg, data_width, '0', mem_data_reg);
      if To_bit(reset) = '1' then
        return;
      end if;
      case data_width is
        when dlx_mem_width_byte =>
          case ls_2_addr_bits'(mem_addr_reg(1 downto 0)) is
            when B"00" =>
              temp(0 to 7) := mem_data_reg(0 to 7);
            when B"01" =>
              temp(0 to 7) := mem_data_reg(8 to 15);
            when B"10" =>
              temp(0 to 7) := mem_data_reg(16 to 23);
            when B"11" =>
              temp(0 to 7) := mem_data_reg(24 to 31);
          end case;
          if unsigned then
            reg(Itype_rd) := bv_zext(temp(0 to 7), 32);
          else
            reg(Itype_rd) := bv_sext(temp(0 to 7), 32);
          end if;
        when dlx_mem_width_halfword =>
          if mem_addr_reg(1) = '0' then
            temp(0 to 15) := mem_data_reg(0 to 15);
          else
            temp(0 to 15) := mem_data_reg(16 to 31);
          end if;
          if unsigned then
            reg(Itype_rd) := bv_zext(temp(0 to 15), 32);
          else
            reg(Itype_rd) := bv_sext(temp(0 to 15), 32);
          end if;
        when dlx_mem_width_word =>
          reg(Itype_rd) := mem_data_reg;
        when others =>
          null;
      end case;
    end procedure execute_load;


    procedure execute_store ( data_width : dlx_mem_width ) is

      variable temp : dlx_bv_word;

      -- type for least-significant two bits of address
      subtype ls_2_addr_bits is bit_vector(1 downto 0);

    begin
      mem_addr_reg := reg(rs1) + bv_sext(IR_immed16, 32);
      mem_data_reg := X"0000_0000";
      case data_width is
        when dlx_mem_width_byte =>
          case ls_2_addr_bits'(mem_addr_reg(1 downto 0)) is
            when B"00" =>
              mem_data_reg(0 to 7) := reg(Itype_rd)(0 to 7);
            when B"01" =>
              mem_data_reg(8 to 15) := reg(Itype_rd)(0 to 7);
            when B"10" =>
              mem_data_reg(16 to 23) := reg(Itype_rd)(0 to 7);
            when B"11" =>
              mem_data_reg(24 to 31) := reg(Itype_rd)(0 to 7);
          end case;
        when dlx_mem_width_halfword =>
          if mem_addr_reg(1) = '0' then
            mem_data_reg(0 to 15) := reg(Itype_rd)(0 to 15);
          else
            mem_data_reg(16 to 31) := reg(Itype_rd)(0 to 15);
          end if;
        when dlx_mem_width_word =>
          mem_data_reg := reg(Itype_rd);
        when others =>
          null;
      end case;
      bus_write(mem_addr_reg, data_width, mem_data_reg);
    end procedure execute_store;


  begin -- interpreter

    -- reset the processor
    d <= disabled_dlx_word;
    halt <= '0';
    write_enable <= '0';
    mem_enable <= '0';
    reg(0) := X"0000_0000";
    PC := X"0000_0000";
    instr_count := 0;
    wait on phi2 until falling_edge(phi2) and To_bit(reset) = '0';

    -- fetch-decode-execute loop
    while To_bit(reset) /= '1' loop
      -- fetch next instruction
      instr_count := instr_count + 1;
      if debug = msg_every_100_instructions and instr_count mod 100 = 0 then
        report "instruction count = " & natural'image(instr_count);
      end if;

      if debug >= msg_each_instruction then
        report "fetching instruction";
      end if;

      bus_read( address => PC, data_width => dlx_mem_width_word,
                instr_fetch => '1', data => IR );
      exit when To_bit(reset) = '1';

      if debug >= trace_each_instruction then
	disassemble(IR, disassembled_instr, disassembled_instr_len);
        report disassembled_instr(1 to disassembled_instr_len);
      end if;

      wait until rising_edge(phi1);

      -- increment the PC to point to the following instruction
      if debug = trace_each_step then
        report "incrementing PC";
      end if;

      PC := bv_addu(PC, PC_incr);

      -- decode the instruction
      if debug = trace_each_step then
        report "decoding instruction";
      end if;

      rs1 := bv_to_natural(IR_rs1);
      rs2 := bv_to_natural(IR_rs2);
      Itype_rd := bv_to_natural(IR_Itype_rd);
      Rtype_rd := bv_to_natural(IR_Rtype_rd);

      -- execute the instruction
      if debug = trace_each_step then
        report "executing instruction";
      end if;

      overflow := false;

      case IR_opcode is
        when op_special =>
          execute_op_special;
        when op_fparith =>
          execute_op_fparith;
        when op_j  => 
          PC := PC + bv_sext(IR_immed26, 32);
        when op_jal => 
          reg(link_reg) := PC;
          PC := PC + bv_sext(IR_immed26, 32);
        when op_jr => 
          PC := reg(rs1);
        when op_jalr => 
          reg(link_reg) := PC;
          PC := reg(rs1);
        when op_beqz => 
          if reg(rs1) = X"0000_0000" then
            PC := PC + bv_sext(IR_immed16, 32);
          end if;
        when op_bnez => 
          if reg(rs1) /= X"0000_0000" then
            PC := PC + bv_sext(IR_immed16, 32);
          end if;
        when op_addi =>
          bv_add(reg(rs1), bv_sext(IR_immed16, 32), reg(Itype_rd), overflow);
        when op_addui => 
          bv_addu(reg(rs1), bv_zext(IR_immed16, 32), reg(Itype_rd), overflow);
        when op_subi => 
          bv_sub(reg(rs1), bv_sext(IR_immed16, 32), reg(Itype_rd), overflow);
        when op_subui => 
          bv_subu(reg(rs1), bv_zext(IR_immed16, 32), reg(Itype_rd), overflow);
        when op_slli => 
          reg(Itype_rd) := reg(rs1) sll bv_to_natural(IR_immed16(11 to 15));
        when op_srli =>
          reg(Itype_rd) := reg(rs1) srl bv_to_natural(IR_immed16(11 to 15));
        when op_srai => 
          reg(Itype_rd) := reg(rs1) sra bv_to_natural(IR_immed16(11 to 15));
        when op_andi => 
          reg(Itype_rd) := reg(rs1) and bv_zext(IR_immed16, 32);
        when op_ori => 
          reg(Itype_rd) := reg(rs1) or bv_zext(IR_immed16, 32);
        when op_xori => 
          reg(Itype_rd) := reg(rs1) xor bv_zext(IR_immed16, 32);
        when op_lhi => 
          reg(Itype_rd) := IR_immed16 & X"0000";
        when op_sequi => 
          reg(Itype_rd) := set_if( reg(rs1) = bv_zext(IR_immed16, 32) );
        when op_sneui => 
          reg(Itype_rd) := set_if( reg(rs1) /= bv_zext(IR_immed16, 32) );
        when op_sltui => 
          reg(Itype_rd) := set_if( reg(rs1) < bv_zext(IR_immed16, 32) );
        when op_sgtui => 
          reg(Itype_rd) := set_if( reg(rs1) > bv_zext(IR_immed16, 32) );
        when op_sleui => 
          reg(Itype_rd) := set_if( reg(rs1) <= bv_zext(IR_immed16, 32) );
        when op_sgeui => 
          reg(Itype_rd) := set_if( reg(rs1) >= bv_zext(IR_immed16, 32) );
        when op_seqi => 
          reg(Itype_rd) := set_if( reg(rs1) = bv_sext(IR_immed16, 32) );
        when op_snei => 
          reg(Itype_rd) := set_if( reg(rs1) /= bv_sext(IR_immed16, 32) );
        when op_slti => 
          reg(Itype_rd) := set_if( bv_lt(reg(rs1), bv_sext(IR_immed16, 32)) );
        when op_sgti => 
          reg(Itype_rd) := set_if( bv_gt(reg(rs1), bv_sext(IR_immed16, 32)) );
        when op_slei => 
          reg(Itype_rd) := set_if( bv_le(reg(rs1), bv_sext(IR_immed16, 32)) );
        when op_sgei => 
          reg(Itype_rd) := set_if( bv_ge(reg(rs1), bv_sext(IR_immed16, 32)) );
        when op_trap =>
          report "TRAP instruction encountered, execution halted" severity note;
          halt <= '1' after Tpd_clk_out;
          wait until To_bit(reset) = '1';
          exit;
        when op_lb =>
          execute_load(data_width => dlx_mem_width_byte, unsigned => false);
          exit when To_bit(reset) = '1';
        when op_lh => 
          execute_load(data_width => dlx_mem_width_halfword, unsigned => false);
          exit when To_bit(reset) = '1';
        when op_lw => 
          execute_load(data_width => dlx_mem_width_word, unsigned => false);
          exit when To_bit(reset) = '1';
        when op_lbu => 
          execute_load(data_width => dlx_mem_width_byte, unsigned => true);
          exit when To_bit(reset) = '1';
        when op_lhu => 
          execute_load(data_width => dlx_mem_width_halfword, unsigned => true);
          exit when To_bit(reset) = '1';
        when op_sb => 
          execute_store ( data_width => dlx_mem_width_byte );
          exit when To_bit(reset) = '1';
        when op_sh => 
          execute_store ( data_width => dlx_mem_width_halfword );
          exit when To_bit(reset) = '1';
        when op_sw => 
          execute_store ( data_width => dlx_mem_width_word );
          exit when To_bit(reset) = '1';
        when op_rfe | op_bfpt | op_bfpf | op_lf | op_ld | op_sf | op_sd => 
          report opcode_names(bv_to_natural(IR_opcode))
            & " instruction not implemented" severity warning;
        when others =>
          report "undefined instruction" severity error;
      end case;

      -- fix up R0 in case it was overwritten
      reg(0) := X"0000_0000";

      -- overflow and divide-by-zero exception handing
      -- (not implemented)

      if debug = trace_each_step then
        report "end of execution";
      end if;

    end loop;
    -- loop is only exited when reset active:
    -- process interpreter starts again from beginning
  end process interpreter;

end architecture behavior;
