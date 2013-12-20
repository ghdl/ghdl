
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
-- $Id: ch_15_dlxi-b.vhd,v 1.3 2001-10-26 16:29:35 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library bv_utilities;

package body dlx_instr is

  use bv_utilities.bv_arithmetic.all;

  constant opcode_names : opcode_name_array
    := ( "SPECIAL ",   "FPARITH ",   "J       ",   "JAL     ",
         "BEQZ    ",   "BNEZ    ",   "BFPT    ",   "BFPF    ",
         "ADDI    ",    "ADDUI   ",   "SUBI    ",   "SUBUI   ",
         "ANDI    ",   "ORI     ",   "XORI    ",   "LHI     ",
         "RFE     ",   "TRAP    ",   "JR      ",   "JALR    ",
         "SLLI    ",   "UNDEF_15",   "SRLI    ",   "SRAI    ",
         "SEQI    ",   "SNEI    ",   "SLTI    ",   "SGTI    ",
         "SLEI    ",   "SGEI    ",   "UNDEF_1E",   "UNDEF_1F",
         "LB      ",   "LH      ",   "UNDEF_22",   "LW      ",
         "LBU     ",   "LHU     ",   "LF      ",   "LD      ",
         "SB      ",   "SH      ",   "UNDEF_2A",   "SW      ",
         "UNDEF_2C",   "UNDEF_2D",   "SF      ",   "SD      ",
         "SEQUI   ",   "SNEUI   ",   "SLTUI   ",   "SGTUI   ",
         "SLEUI   ",   "SGEUI   ",   "UNDEF_36",   "UNDEF_37",
         "UNDEF_38",   "UNDEF_39",   "UNDEF_3A",   "UNDEF_3B",
         "UNDEF_3C",   "UNDEF_3D",   "UNDEF_3E",   "UNDEF_3F" );

  constant sp_func_names : sp_func_name_array
    := ( "NOP     ",   "UNDEF_01",   "UNDEF_02",   "UNDEF_03",
         "SLL     ",   "UNDEF_05",   "SRL     ",   "SRA     ",
         "UNDEF_08",   "UNDEF_09",   "UNDEF_0A",   "UNDEF_0B",
         "UNDEF_0C",   "UNDEF_0D",   "UNDEF_0E",   "UNDEF_0F",
         "SEQU    ",   "SNEU    ",   "SLTU    ",   "SGTU    ",
         "SLEU    ",   "SGEU    ",   "UNDEF_16",   "UNDEF_17",
         "UNDEF_18",   "UNDEF_19",   "UNDEF_1A",   "UNDEF_1B",
         "UNDEF_1C",   "UNDEF_1D",   "UNDEF_1E",   "UNDEF_1F",
         "ADD     ",   "ADDU    ",   "SUB     ",   "SUBU    ",
         "AND     ",   "OR      ",   "XOR     ",   "UNDEF_27",
         "SEQ     ",   "SNE     ",   "SLT     ",   "SGT     ",
         "SLE     ",   "SGE     ",   "UNDEF_2E",   "UNDEF_2F",
         "MOVI2S  ",   "MOVS2I  ",   "MOVF    ",   "MOVD    ",
         "MOVFP2I ",   "MOVI2FP ",   "UNDEF_36",   "UNDEF_37",
         "UNDEF_38",   "UNDEF_39",   "UNDEF_3A",   "UNDEF_3B",
         "UNDEF_3C",   "UNDEF_3D",   "UNDEF_3E",   "UNDEF_3F" );

  constant fp_func_names : fp_func_name_array
    := ( "ADDF    ",   "SUBF    ",   "MULTF   ",   "DIVF    ",
         "ADDD    ",   "SUBD    ",   "MULTD   ",   "DIVD    ",
         "CVTF2D  ",   "CVTF2I  ",   "CVTD2F  ",   "CVTD2I  ",
         "CVTI2F  ",   "CVTI2D  ",   "MULT    ",   "DIV     ",
         "EQF     ",   "NEF     ",   "LTF     ",   "GTF     ",
         "LEF     ",   "GEF     ",   "MULTU   ",   "DIVU    ",
         "EQD     ",   "NED     ",   "LTD     ",   "GTD     ",
         "LED     ",   "GED     ",   "UNDEF_1E",   "UNDEF_1F" );


  procedure disassemble ( instr : dlx_bv_word;
			  disassembled_instr : out string;  len : out positive ) is

    alias norm_disassembled_instr : string(1 to disassembled_instr'length)
      is disassembled_instr;

    alias instr_opcode : dlx_opcode is instr(0 to 5);
    alias instr_sp_func : dlx_sp_func is instr(26 to 31);
    alias instr_fp_func : dlx_fp_func is instr(27 to 31);
    alias instr_rs1 : dlx_reg_addr is instr(6 to 10);
    alias instr_rs2 : dlx_reg_addr is instr(11 to 15);
    alias instr_Itype_rd : dlx_reg_addr is instr(11 to 15);
    alias instr_Rtype_rd : dlx_reg_addr is instr(16 to 20);
    alias instr_immed16 : dlx_immed16 is instr(16 to 31);
    alias instr_immed26 : dlx_immed26 is instr(6 to 31);

    variable instr_opcode_num : dlx_opcode_num;
    variable instr_sp_func_num : dlx_sp_func_num;
    variable instr_fp_func_num : dlx_fp_func_num;
    variable rs1 : reg_index;
    variable rs2 : reg_index;
    variable Itype_rd : reg_index;
    variable Rtype_rd : reg_index;
    variable result : string(1 to 40)	-- long enough for longest instruction
		:= (others => ' ');
    variable index : positive range 1 to 41 := 1;  -- position for next char in result

    procedure disassemble_reg ( reg : reg_index;  reg_prefix : character ) is
    begin
      result(index) := reg_prefix;
      index := index + 1;
      if reg < 10 then
        result(index to index) := integer'image(reg);
        index := index + 1;
      else
        result(index to index + 1) := integer'image(reg);
        index := index + 2;
      end if;
    end procedure disassemble_reg;

    procedure disassemble_special_reg ( reg : reg_index ) is
    begin
      case reg is
        when 0 =>
          result(index to index + 2) := "IAR";
          index := index + 3;
        when 1 =>
          result(index to index + 2) := "FSR";
          index := index + 3;
        when others =>
          disassemble_reg(reg, 'S');
      end case;
    end procedure disassemble_special_reg;

    procedure disassemble_integer ( int : integer ) is
      constant int_image_length : natural := integer'image(int)'length;
    begin
      result(index to index + int_image_length - 1) := integer'image(int);
      index := index + int_image_length;
    end procedure disassemble_integer;

  begin
    instr_opcode_num := bv_to_natural(instr_opcode);
    instr_sp_func_num := bv_to_natural(instr_sp_func);
    instr_fp_func_num := bv_to_natural(instr_fp_func);
    rs1 := bv_to_natural(instr_rs1);
    rs2 := bv_to_natural(instr_rs2);
    Itype_rd := bv_to_natural(instr_Itype_rd);
    Rtype_rd := bv_to_natural(instr_Rtype_rd);
    if (instr_opcode /= op_special) and (instr_opcode /= op_fparith) then
      result(index to index + instr_name'length - 1) := opcode_names(instr_opcode_num);
      index := index + instr_name'length + 1;  -- include space after opcode name
    end if;
    case instr_opcode is
      when op_special =>
	result(index to index + instr_name'length - 1) := sp_func_names(instr_sp_func_num);
        index := index + instr_name'length + 1;  -- include space after function name
        case instr_sp_func is
          when sp_func_nop =>
            null;
          when sp_func_sll | sp_func_srl | sp_func_sra
            | sp_func_sequ | sp_func_sneu | sp_func_sltu
            | sp_func_sgtu | sp_func_sleu | sp_func_sgeu
            | sp_func_add | sp_func_addu | sp_func_sub | sp_func_subu
            | sp_func_and | sp_func_or | sp_func_xor
            | sp_func_seq | sp_func_sne | sp_func_slt
            | sp_func_sgt | sp_func_sle | sp_func_sge =>
	    disassemble_reg(Rtype_rd, 'R');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'R');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs2, 'R');
          when sp_func_movi2s =>
	    disassemble_special_reg(Rtype_rd);
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'R');
          when sp_func_movs2i =>
	    disassemble_reg(Rtype_rd, 'R');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_special_reg(rs1);
          when sp_func_movf | sp_func_movd =>
	    disassemble_reg(Rtype_rd, 'F');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'F');
          when sp_func_movfp2i =>
	    disassemble_reg(Rtype_rd, 'R');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'F');
          when sp_func_movi2fp =>
	    disassemble_reg(Rtype_rd, 'F');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'R');
          when others =>
            null;
        end case;
      when op_fparith =>
	result(index to index + instr_name'length - 1) := fp_func_names(instr_fp_func_num);
        index := index + instr_name'length + 1;  -- include space after function name
        case instr_fp_func is
          when fp_func_addf | fp_func_subf | fp_func_multf | fp_func_divf
            | fp_func_addd | fp_func_subd | fp_func_multd | fp_func_divd
            | fp_func_mult | fp_func_div | fp_func_multu | fp_func_divu =>
	    disassemble_reg(Rtype_rd, 'F');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'F');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs2, 'F');
          when fp_func_cvtf2d | fp_func_cvtd2f =>
	    disassemble_reg(Rtype_rd, 'F');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'F');
          when fp_func_cvtf2i | fp_func_cvtd2i =>
	    disassemble_reg(Rtype_rd, 'R');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'F');
          when fp_func_cvti2f | fp_func_cvti2d =>
	    disassemble_reg(Rtype_rd, 'F');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs1, 'R');
          when fp_func_eqf | fp_func_nef | fp_func_ltf
            | fp_func_gtf | fp_func_lef | fp_func_gef
            | fp_func_eqd | fp_func_ned | fp_func_ltd
            | fp_func_gtd | fp_func_led | fp_func_ged =>
	    disassemble_reg(rs1, 'F');
	    result(index) := ',';
	    index := index + 2;  -- include space after comma
	    disassemble_reg(rs2, 'F');
          when others =>
            null;
        end case;
      when op_j  | op_jal =>
        disassemble_integer(bv_to_integer(instr_immed26));
      when op_beqz | op_bnez =>
	disassemble_reg(rs1, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_integer(bv_to_integer(instr_immed16));
      when op_bfpt | op_bfpf =>
	disassemble_integer(bv_to_integer(instr_immed16));
      when op_slli | op_srli | op_srai =>
	disassemble_reg(Itype_rd, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_reg(rs1, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_integer(bv_to_natural(instr_immed16(11 to 15)));
      when op_addi | op_subi
        | op_seqi | op_snei | op_slti | op_sgti | op_slei | op_sgei =>
	disassemble_reg(Itype_rd, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_reg(rs1, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_integer(bv_to_integer(instr_immed16));
      when op_addui | op_subui | op_andi | op_ori | op_xori
        | op_sequi | op_sneui | op_sltui | op_sgtui | op_sleui | op_sgeui =>
	disassemble_reg(Itype_rd, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_reg(rs1, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_integer(bv_to_natural(instr_immed16));
      when op_lhi =>
	disassemble_reg(Itype_rd, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_integer(bv_to_natural(instr_immed16));
      when op_rfe =>
        null;
      when op_trap =>
	disassemble_integer(bv_to_natural(instr_immed26));
      when op_jr | op_jalr =>
	disassemble_reg(rs1, 'R');
      when op_lb | op_lh | op_lw | op_lbu | op_lhu | op_lf | op_ld =>
	disassemble_reg(Itype_rd, 'R');
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_integer(bv_to_integer(instr_immed16));
        result(index) := '(';
                           index := index + 1;
                           disassemble_reg(rs1, 'R');
                           result(index) := ')';
	index := index + 1;
      when op_sb | op_sh | op_sw | op_sf | op_sd =>
	disassemble_integer(bv_to_integer(instr_immed16));
        result(index) := '(';
                           index := index + 1;
                           disassemble_reg(rs1, 'R');
                           result(index) := ')';
	index := index + 1;
	result(index) := ',';
	index := index + 2;  -- include space after comma
	disassemble_reg(Itype_rd, 'R');
      when others =>
        null;  -- remaining opcodes have no operands to disassemble
    end case;
    if index > norm_disassembled_instr'length then
      index := norm_disassembled_instr'length;  -- limit to out parameter length
    else
      index := index - 1;  -- index points to last result character
    end if;
    norm_disassembled_instr(1 to index) := result(1 to index);
    len := index;
  end procedure disassemble;

end package body dlx_instr;
