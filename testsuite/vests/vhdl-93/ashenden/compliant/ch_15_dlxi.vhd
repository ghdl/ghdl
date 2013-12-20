
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
-- $Id: ch_15_dlxi.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

use work.dlx_types.all;

package dlx_instr is

  subtype dlx_opcode is bit_vector(0 to 5);
  subtype dlx_sp_func is bit_vector(0 to 5);
  subtype dlx_fp_func is bit_vector(0 to 4);
  subtype dlx_reg_addr is bit_vector(0 to 4);
  subtype dlx_immed16 is bit_vector(0 to 15);
  subtype dlx_immed26 is bit_vector(0 to 25);

  constant op_special   : dlx_opcode := B"000000";
  constant op_fparith   : dlx_opcode := B"000001";
  constant op_j         : dlx_opcode := B"000010";
  constant op_jal       : dlx_opcode := B"000011";
  constant op_beqz      : dlx_opcode := B"000100";
  constant op_bnez      : dlx_opcode := B"000101";
  constant op_bfpt      : dlx_opcode := B"000110";
  constant op_bfpf      : dlx_opcode := B"000111";
  constant op_addi      : dlx_opcode := B"001000";
  constant op_addui     : dlx_opcode := B"001001";
  constant op_subi	: dlx_opcode := B"001010";
  constant op_subui	: dlx_opcode := B"001011";
  constant op_andi	: dlx_opcode := B"001100";
  constant op_ori	: dlx_opcode := B"001101";
  constant op_xori	: dlx_opcode := B"001110";
  constant op_lhi	: dlx_opcode := B"001111";

  constant op_rfe	: dlx_opcode := B"010000";
  constant op_trap	: dlx_opcode := B"010001";
  constant op_jr	: dlx_opcode := B"010010";
  constant op_jalr	: dlx_opcode := B"010011";
  constant op_slli	: dlx_opcode := B"010100";
  constant op_undef_15	: dlx_opcode := B"010101";
  constant op_srli	: dlx_opcode := B"010110";
  constant op_srai	: dlx_opcode := B"010111";
  constant op_seqi	: dlx_opcode := B"011000";
  constant op_snei	: dlx_opcode := B"011001";
  constant op_slti	: dlx_opcode := B"011010";
  constant op_sgti	: dlx_opcode := B"011011";
  constant op_slei	: dlx_opcode := B"011100";
  constant op_sgei	: dlx_opcode := B"011101";
  constant op_undef_1E	: dlx_opcode := B"011110";
  constant op_undef_1F	: dlx_opcode := B"011111";

  constant op_lb	: dlx_opcode := B"100000";
  constant op_lh	: dlx_opcode := B"100001";
  constant op_undef_22	: dlx_opcode := B"100010";
  constant op_lw	: dlx_opcode := B"100011";
  constant op_lbu	: dlx_opcode := B"100100";
  constant op_lhu	: dlx_opcode := B"100101";
  constant op_lf	: dlx_opcode := B"100110";
  constant op_ld	: dlx_opcode := B"100111";
  constant op_sb	: dlx_opcode := B"101000";
  constant op_sh	: dlx_opcode := B"101001";
  constant op_undef_2A	: dlx_opcode := B"101010";
  constant op_sw	: dlx_opcode := B"101011";
  constant op_undef_2C	: dlx_opcode := B"101100";
  constant op_undef_2D	: dlx_opcode := B"101101";
  constant op_sf	: dlx_opcode := B"101110";
  constant op_sd	: dlx_opcode := B"101111";

  constant op_sequi	: dlx_opcode := B"110000";
  constant op_sneui	: dlx_opcode := B"110001";
  constant op_sltui	: dlx_opcode := B"110010";
  constant op_sgtui	: dlx_opcode := B"110011";
  constant op_sleui	: dlx_opcode := B"110100";
  constant op_sgeui	: dlx_opcode := B"110101";
  constant op_undef_36	: dlx_opcode := B"110110";
  constant op_undef_37	: dlx_opcode := B"110111";
  constant op_undef_38	: dlx_opcode := B"111000";
  constant op_undef_39	: dlx_opcode := B"111001";
  constant op_undef_3A	: dlx_opcode := B"111010";
  constant op_undef_3B	: dlx_opcode := B"111011";
  constant op_undef_3C	: dlx_opcode := B"111100";
  constant op_undef_3D	: dlx_opcode := B"111101";
  constant op_undef_3E	: dlx_opcode := B"111110";
  constant op_undef_3F  : dlx_opcode := B"111111";

  constant sp_func_nop       : dlx_sp_func := B"000000";
  constant sp_func_undef_01  : dlx_sp_func := B"000001";
  constant sp_func_undef_02  : dlx_sp_func := B"000010";
  constant sp_func_undef_03  : dlx_sp_func := B"000011";
  constant sp_func_sll       : dlx_sp_func := B"000100";
  constant sp_func_undef_05  : dlx_sp_func := B"000101";
  constant sp_func_srl	     : dlx_sp_func := B"000110";
  constant sp_func_sra	     : dlx_sp_func := B"000111";
  constant sp_func_undef_08  : dlx_sp_func := B"001000";
  constant sp_func_undef_09  : dlx_sp_func := B"001001";
  constant sp_func_undef_0A  : dlx_sp_func := B"001010";
  constant sp_func_undef_0B  : dlx_sp_func := B"001011";
  constant sp_func_undef_0C  : dlx_sp_func := B"001100";
  constant sp_func_undef_0D  : dlx_sp_func := B"001101";
  constant sp_func_undef_0E  : dlx_sp_func := B"001110";
  constant sp_func_undef_0F  : dlx_sp_func := B"001111";
  
  constant sp_func_sequ	     : dlx_sp_func := B"010000";
  constant sp_func_sneu	     : dlx_sp_func := B"010001";
  constant sp_func_sltu	     : dlx_sp_func := B"010010";
  constant sp_func_sgtu	     : dlx_sp_func := B"010011";
  constant sp_func_sleu	     : dlx_sp_func := B"010100";
  constant sp_func_sgeu	     : dlx_sp_func := B"010101";
  constant sp_func_undef_16  : dlx_sp_func := B"010110";
  constant sp_func_undef_17  : dlx_sp_func := B"010111";
  constant sp_func_undef_18  : dlx_sp_func := B"011000";
  constant sp_func_undef_19  : dlx_sp_func := B"011001";
  constant sp_func_undef_1A  : dlx_sp_func := B"011010";
  constant sp_func_undef_1B  : dlx_sp_func := B"011011";
  constant sp_func_undef_1C  : dlx_sp_func := B"011100";
  constant sp_func_undef_1D  : dlx_sp_func := B"011101";
  constant sp_func_undef_1E  : dlx_sp_func := B"011110";
  constant sp_func_undef_1F  : dlx_sp_func := B"011111";
  
  constant sp_func_add	     : dlx_sp_func := B"100000";
  constant sp_func_addu	     : dlx_sp_func := B"100001";
  constant sp_func_sub	     : dlx_sp_func := B"100010";
  constant sp_func_subu	     : dlx_sp_func := B"100011";
  constant sp_func_and	     : dlx_sp_func := B"100100";
  constant sp_func_or	     : dlx_sp_func := B"100101";
  constant sp_func_xor	     : dlx_sp_func := B"100110";
  constant sp_func_undef_27  : dlx_sp_func := B"100111";
  constant sp_func_seq	     : dlx_sp_func := B"101000";
  constant sp_func_sne	     : dlx_sp_func := B"101001";
  constant sp_func_slt	     : dlx_sp_func := B"101010";
  constant sp_func_sgt	     : dlx_sp_func := B"101011";
  constant sp_func_sle	     : dlx_sp_func := B"101100";
  constant sp_func_sge	     : dlx_sp_func := B"101101";
  constant sp_func_undef_2E  : dlx_sp_func := B"101110";
  constant sp_func_undef_2F  : dlx_sp_func := B"101111";
  
  constant sp_func_movi2s    : dlx_sp_func := B"110000";
  constant sp_func_movs2i    : dlx_sp_func := B"110001";
  constant sp_func_movf	     : dlx_sp_func := B"110010";
  constant sp_func_movd	     : dlx_sp_func := B"110011";
  constant sp_func_movfp2i   : dlx_sp_func := B"110100";
  constant sp_func_movi2fp   : dlx_sp_func := B"110101";
  constant sp_func_undef_36  : dlx_sp_func := B"110110";
  constant sp_func_undef_37  : dlx_sp_func := B"110111";
  constant sp_func_undef_38  : dlx_sp_func := B"111000";
  constant sp_func_undef_39  : dlx_sp_func := B"111001";
  constant sp_func_undef_3A  : dlx_sp_func := B"111010";
  constant sp_func_undef_3B  : dlx_sp_func := B"111011";
  constant sp_func_undef_3C  : dlx_sp_func := B"111100";
  constant sp_func_undef_3D  : dlx_sp_func := B"111101";
  constant sp_func_undef_3E  : dlx_sp_func := B"111110";
  constant sp_func_undef_3F  : dlx_sp_func := B"111111";
  
  constant fp_func_addf      : dlx_fp_func := B"00000";
  constant fp_func_subf      : dlx_fp_func := B"00001";
  constant fp_func_multf     : dlx_fp_func := B"00010";
  constant fp_func_divf	     : dlx_fp_func := B"00011";
  constant fp_func_addd	     : dlx_fp_func := B"00100";
  constant fp_func_subd	     : dlx_fp_func := B"00101";
  constant fp_func_multd     : dlx_fp_func := B"00110";
  constant fp_func_divd	     : dlx_fp_func := B"00111";
  constant fp_func_cvtf2d    : dlx_fp_func := B"01000";
  constant fp_func_cvtf2i    : dlx_fp_func := B"01001";
  constant fp_func_cvtd2f    : dlx_fp_func := B"01010";
  constant fp_func_cvtd2i    : dlx_fp_func := B"01011";
  constant fp_func_cvti2f    : dlx_fp_func := B"01100";
  constant fp_func_cvti2d    : dlx_fp_func := B"01101";
  constant fp_func_mult	     : dlx_fp_func := B"01110";
  constant fp_func_div	     : dlx_fp_func := B"01111";
  
  constant fp_func_eqf	     : dlx_fp_func := B"10000";
  constant fp_func_nef	     : dlx_fp_func := B"10001";
  constant fp_func_ltf	     : dlx_fp_func := B"10010";
  constant fp_func_gtf	     : dlx_fp_func := B"10011";
  constant fp_func_lef	     : dlx_fp_func := B"10100";
  constant fp_func_gef	     : dlx_fp_func := B"10101";
  constant fp_func_multu     : dlx_fp_func := B"10110";
  constant fp_func_divu	     : dlx_fp_func := B"10111";
  constant fp_func_eqd	     : dlx_fp_func := B"11000";
  constant fp_func_ned	     : dlx_fp_func := B"11001";
  constant fp_func_ltd	     : dlx_fp_func := B"11010";
  constant fp_func_gtd	     : dlx_fp_func := B"11011";
  constant fp_func_led	     : dlx_fp_func := B"11100";
  constant fp_func_ged	     : dlx_fp_func := B"11101";
  constant fp_func_undef_1E  : dlx_fp_func := B"11110";
  constant fp_func_undef_1F  : dlx_fp_func := B"11111";

  subtype dlx_opcode_num is natural range 0 to 63;
  subtype dlx_sp_func_num is natural range 0 to 63;
  subtype dlx_fp_func_num is natural range 0 to 31;

  subtype instr_name is string(1 to 8);
  type opcode_name_array is array (dlx_opcode_num) of instr_name;
  type sp_func_name_array is array (dlx_sp_func_num) of instr_name;
  type fp_func_name_array is array (dlx_fp_func_num) of instr_name;

  constant opcode_names : opcode_name_array;
  constant sp_func_names : sp_func_name_array;
  constant fp_func_names : fp_func_name_array;

  subtype reg_index is natural range 0 to 31;

  constant link_reg : reg_index := 31;

  procedure disassemble ( instr : dlx_bv_word;
			  disassembled_instr : out string;  len : out positive );

end package dlx_instr;
