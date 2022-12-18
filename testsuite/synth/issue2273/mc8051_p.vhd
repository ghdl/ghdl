-------------------------------------------------------------------------------
--                                                                           --
--          X       X   XXXXXX    XXXXXX    XXXXXX    XXXXXX      X          --
--          XX     XX  X      X  X      X  X      X  X           XX          --
--          X X   X X  X         X      X  X      X  X          X X          --
--          X  X X  X  X         X      X  X      X  X         X  X          --
--          X   X   X  X          XXXXXX   X      X   XXXXXX      X          --
--          X       X  X         X      X  X      X         X     X          --
--          X       X  X         X      X  X      X         X     X          --
--          X       X  X      X  X      X  X      X         X     X          --
--          X       X   XXXXXX    XXXXXX    XXXXXX    XXXXXX      X          --
--                                                                           --
--                                                                           --
--                       O R E G A N O   S Y S T E M S                       --
--                                                                           --
--                            Design & Consulting                            --
--                                                                           --
-------------------------------------------------------------------------------
--                                                                           --
--         Web:           http://www.oregano.at/                             --
--                                                                           --
--         Contact:       mc8051@oregano.at                                  --
--                                                                           --
-------------------------------------------------------------------------------
--                                                                           --
--  MC8051 - VHDL 8051 Microcontroller IP Core                               --
--  Copyright (C) 2001 OREGANO SYSTEMS                                       --
--                                                                           --
--  This library is free software; you can redistribute it and/or            --
--  modify it under the terms of the GNU Lesser General Public               --
--  License as published by the Free Software Foundation; either             --
--  version 2.1 of the License, or (at your option) any later version.       --
--                                                                           --
--  This library is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        --
--  Lesser General Public License for more details.                          --
--                                                                           --
--  Full details of the license can be found in the file LGPL.TXT.           --
--                                                                           --
--  You should have received a copy of the GNU Lesser General Public         --
--  License along with this library; if not, write to the Free Software      --
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  --
--                                                                           --
-------------------------------------------------------------------------------
--
--
--         Author:                 Roland Höller
--
--         Filename:               mc8051_p.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.16 $
--
--         Date of Latest Version: $Date: 2006-09-07 10:29:29 $
--
--
--         Description: Collection of constants, types, and components.
--
--
--
--
-------------------------------------------------------------------------------
library IEEE; 
use IEEE.std_logic_1164.all; 
use IEEE.std_logic_arith.all; 

package mc8051_p is

  -----------------------------------------------------------------------------
  -- Set data width of mc8051_alu (no other than 8 supported at the moment!)
  -- Default: 8
  constant C_DWIDTH : integer := 8;
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Select whether to implement (1) or skip (0) the multiplier
  -- Default: 1
  constant C_IMPL_MUL : integer := 1;
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Select whether to implement (1) or skip (0) the divider
  -- Default: 1
  constant C_IMPL_DIV : integer := 1;
  -----------------------------------------------------------------------------
           
  -----------------------------------------------------------------------------
  -- Select whether to implement (1) or skip (0) the decimal adjustment command
  -- Default: 1
  constant C_IMPL_DA  : integer := 1;
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Select how many timer/counter units should be implemented
  -- Default: 1
  constant C_IMPL_N_TMR : integer := 1;
  -----------------------------------------------------------------------------

  -----------------------------------------------------------------------------
  -- Select how many serial interface units should be implemented
  -- Default: C_IMPL_N_TMR ---(DO NOT CHANGE!)---        
  constant C_IMPL_N_SIU : integer := C_IMPL_N_TMR;
  -----------------------------------------------------------------------------
             
  -----------------------------------------------------------------------------
  -- Select how many external interrupt-inputs should be implemented
  -- Default: C_IMPL_N_TMR ---(DO NOT CHANGE!)---
  constant C_IMPL_N_EXT : integer := C_IMPL_N_TMR;
  -----------------------------------------------------------------------------

  constant OFF            : std_logic_vector(5 downto 0) := "000000";
  constant DA             : std_logic_vector(5 downto 0) := "100000";
  constant ADD_ACC_RAM    : std_logic_vector(5 downto 0) := "100001";
  constant ADD_ACC_ROM    : std_logic_vector(5 downto 0) := "100010";
  constant ADDC_ACC_RAM   : std_logic_vector(5 downto 0) := "100011";
  constant ADDC_ACC_ROM   : std_logic_vector(5 downto 0) := "100100";
  constant AND_ACC_RAM    : std_logic_vector(5 downto 0) := "100101";
  constant AND_ACC_ROM    : std_logic_vector(5 downto 0) := "100110";
  constant AND_RAM_ROM    : std_logic_vector(5 downto 0) := "100111";
  constant SUB_ACC_RAM    : std_logic_vector(5 downto 0) := "101000";
  constant SUB_ACC_ROM    : std_logic_vector(5 downto 0) := "101001";
  constant MUL_ACC_RAM    : std_logic_vector(5 downto 0) := "101010";
  constant DIV_ACC_RAM    : std_logic_vector(5 downto 0) := "101011";
  constant OR_RAM_ACC     : std_logic_vector(5 downto 0) := "101100";
  constant OR_ROM_ACC     : std_logic_vector(5 downto 0) := "101101";
  constant OR_ROM_RAM     : std_logic_vector(5 downto 0) := "101110";
  constant XOR_RAM_ACC    : std_logic_vector(5 downto 0) := "101111";
  constant XOR_ROM_ACC    : std_logic_vector(5 downto 0) := "110000";
  constant XOR_ROM_RAM    : std_logic_vector(5 downto 0) := "110001";
  constant RL_ACC         : std_logic_vector(5 downto 0) := "110010";
  constant RLC_ACC        : std_logic_vector(5 downto 0) := "110011";
  constant RR_ACC         : std_logic_vector(5 downto 0) := "110100";
  constant RRC_ACC        : std_logic_vector(5 downto 0) := "110101";
  constant INV_ACC        : std_logic_vector(5 downto 0) := "110110";
  constant INV_RAM        : std_logic_vector(5 downto 0) := "110111";
  constant DEC_ACC        : std_logic_vector(5 downto 0) := "111000";
  constant DEC_RAM        : std_logic_vector(5 downto 0) := "111001";
  constant COMP_RAM_ACC   : std_logic_vector(5 downto 0) := "111010";
  constant COMP_ROM_ACC   : std_logic_vector(5 downto 0) := "111011";
  constant COMP_ROM_RAM   : std_logic_vector(5 downto 0) := "111100";
  constant INC_ACC        : std_logic_vector(5 downto 0) := "111110";
  constant INC_RAM        : std_logic_vector(5 downto 0) := "111111";

  constant ACALL         : std_logic_vector(4 downto 0) :=    "10001";
  constant ADD_A_RR      : std_logic_vector(4 downto 0) := "00101";
  constant ADD_A_D       : std_logic_vector(7 downto 0) := "00100101";
  constant ADD_A_ATRI    : std_logic_vector(6 downto 0) := "0010011";
  constant ADD_A_DATA    : std_logic_vector(7 downto 0) := "00100100";
  constant ADDC_A_RR     : std_logic_vector(4 downto 0) := "00111";
  constant ADDC_A_D      : std_logic_vector(7 downto 0) := "00110101";
  constant ADDC_A_ATRI   : std_logic_vector(6 downto 0) := "0011011";
  constant ADDC_A_DATA   : std_logic_vector(7 downto 0) := "00110100";
  constant AJMP          : std_logic_vector(4 downto 0) :=    "00001";
  constant ANL_A_RR      : std_logic_vector(4 downto 0) := "01011";
  constant ANL_A_D       : std_logic_vector(7 downto 0) := "01010101";
  constant ANL_A_ATRI    : std_logic_vector(6 downto 0) := "0101011";
  constant ANL_A_DATA    : std_logic_vector(7 downto 0) := "01010100";
  constant ANL_D_A       : std_logic_vector(7 downto 0) := "01010010";
  constant ANL_D_DATA    : std_logic_vector(7 downto 0) := "01010011";
  constant ANL_C_BIT     : std_logic_vector(7 downto 0) := "10000010";
  constant ANL_C_NBIT    : std_logic_vector(7 downto 0) := "10110000";
  constant CJNE_A_D      : std_logic_vector(7 downto 0) := "10110101";
  constant CJNE_A_DATA   : std_logic_vector(7 downto 0) := "10110100";
  constant CJNE_RR_DATA  : std_logic_vector(4 downto 0) := "10111";
  constant CJNE_ATRI_DATA: std_logic_vector(6 downto 0) := "1011011" ;
  constant CLR_A         : std_logic_vector(7 downto 0) := "11100100";
  constant CLR_C         : std_logic_vector(7 downto 0) := "11000011";
  constant CLR_BIT       : std_logic_vector(7 downto 0) := "11000010";
  constant CPL_A         : std_logic_vector(7 downto 0) := "11110100";
  constant CPL_C         : std_logic_vector(7 downto 0) := "10110011";
  constant CPL_BIT       : std_logic_vector(7 downto 0) := "10110010";
  constant DA_A          : std_logic_vector(7 downto 0) := "11010100";
  constant DEC_A         : std_logic_vector(7 downto 0) := "00010100";
  constant DEC_RR        : std_logic_vector(4 downto 0) := "00011";
  constant DEC_D         : std_logic_vector(7 downto 0) := "00010101";
  constant DEC_ATRI      : std_logic_vector(6 downto 0) := "0001011";
  constant DIV_AB        : std_logic_vector(7 downto 0) := "10000100";
  constant DJNZ_RR       : std_logic_vector(4 downto 0) := "11011";
  constant DJNZ_D        : std_logic_vector(7 downto 0) := "11010101";
  constant INC_A         : std_logic_vector(7 downto 0) := "00000100";
  constant INC_RR        : std_logic_vector(4 downto 0) := "00001";
  constant INC_D         : std_logic_vector(7 downto 0) := "00000101";
  constant INC_ATRI      : std_logic_vector(6 downto 0) := "0000011";
  constant INC_DPTR      : std_logic_vector(7 downto 0) := "10100011";
  constant JB            : std_logic_vector(7 downto 0) := "00100000";
  constant JBC           : std_logic_vector(7 downto 0) := "00010000";
  constant JC            : std_logic_vector(7 downto 0) := "01000000";
  constant JMP_A_DPTR    : std_logic_vector(7 downto 0) := "01110011";
  constant JNB           : std_logic_vector(7 downto 0) := "00110000";
  constant JNC           : std_logic_vector(7 downto 0) := "01010000";
  constant JNZ           : std_logic_vector(7 downto 0) := "01110000";
  constant JZ            : std_logic_vector(7 downto 0) := "01100000";
  constant LCALL         : std_logic_vector(7 downto 0) := "00010010";
  constant LJMP          : std_logic_vector(7 downto 0) := "00000010";
  constant MOV_A_RR      : std_logic_vector(4 downto 0) := "11101";
  constant MOV_A_D       : std_logic_vector(7 downto 0) := "11100101";
  constant MOV_A_ATRI    : std_logic_vector(6 downto 0) := "1110011";
  constant MOV_A_DATA    : std_logic_vector(7 downto 0) := "01110100";
  constant MOV_RR_A      : std_logic_vector(4 downto 0) := "11111";
  constant MOV_RR_D      : std_logic_vector(4 downto 0) := "10101";
  constant MOV_RR_DATA   : std_logic_vector(4 downto 0) := "01111";
  constant MOV_D_A       : std_logic_vector(7 downto 0) := "11110101";
  constant MOV_D_RR      : std_logic_vector(4 downto 0) := "10001";
  constant MOV_D_D       : std_logic_vector(7 downto 0) := "10000101";
  constant MOV_D_ATRI    : std_logic_vector(6 downto 0) := "1000011";
  constant MOV_D_DATA    : std_logic_vector(7 downto 0) := "01110101";
  constant MOV_ATRI_A    : std_logic_vector(6 downto 0) := "1111011";
  constant MOV_ATRI_D    : std_logic_vector(6 downto 0) := "1010011";
  constant MOV_ATRI_DATA : std_logic_vector(6 downto 0) := "0111011";
  constant MOVC_A_ATDPTR : std_logic_vector(7 downto 0) := "10010011";
  constant MOVC_A_ATPC   : std_logic_vector(7 downto 0) := "10000011";
  constant MOVX_A_ATRI   : std_logic_vector(6 downto 0) := "1110001";
  constant MOVX_A_ATDPTR : std_logic_vector(7 downto 0) := "11100000";
  constant MOVX_ATRI_A   : std_logic_vector(6 downto 0) := "1111001";
  constant MOVX_ATDPTR_A : std_logic_vector(7 downto 0) := "11110000";
  constant MOV_C_BIT     : std_logic_vector(7 downto 0) := "10100010";
  constant MOV_BIT_C     : std_logic_vector(7 downto 0) := "10010010";
  constant MOV_DPTR_DATA : std_logic_vector(7 downto 0) := "10010000";
  constant MUL_AB        : std_logic_vector(7 downto 0) := "10100100";
  constant NOP           : std_logic_vector(7 downto 0) := "00000000";
  constant ORL_A_RR      : std_logic_vector(4 downto 0) := "01001";
  constant ORL_A_D       : std_logic_vector(7 downto 0) := "01000101";
  constant ORL_A_ATRI    : std_logic_vector(6 downto 0) := "0100011";
  constant ORL_A_DATA    : std_logic_vector(7 downto 0) := "01000100";
  constant ORL_D_A       : std_logic_vector(7 downto 0) := "01000010";
  constant ORL_D_DATA    : std_logic_vector(7 downto 0) := "01000011";
  constant ORL_C_BIT     : std_logic_vector(7 downto 0) := "01110010";
  constant ORL_C_NBIT    : std_logic_vector(7 downto 0) := "10100000";
  constant POP           : std_logic_vector(7 downto 0) := "11010000";
  constant PUSH          : std_logic_vector(7 downto 0) := "11000000";
  constant RET           : std_logic_vector(7 downto 0) := "00100010";
  constant RETI          : std_logic_vector(7 downto 0) := "00110010";
  constant RL_A          : std_logic_vector(7 downto 0) := "00100011";
  constant RLC_A         : std_logic_vector(7 downto 0) := "00110011";
  constant RR_A          : std_logic_vector(7 downto 0) := "00000011";
  constant RRC_A         : std_logic_vector(7 downto 0) := "00010011";
  constant SETB_C        : std_logic_vector(7 downto 0) := "11010011";
  constant SETB_BIT      : std_logic_vector(7 downto 0) := "11010010";
  constant SJMP          : std_logic_vector(7 downto 0) := "10000000";
  constant SUBB_A_RR     : std_logic_vector(4 downto 0) := "10011";
  constant SUBB_A_D      : std_logic_vector(7 downto 0) := "10010101";
  constant SUBB_A_ATRI   : std_logic_vector(6 downto 0) := "1001011";
  constant SUBB_A_DATA   : std_logic_vector(7 downto 0) := "10010100";
  constant SWAP_A        : std_logic_vector(7 downto 0) := "11000100";
  constant XCH_A_RR      : std_logic_vector(4 downto 0) := "11001";
  constant XCH_A_D       : std_logic_vector(7 downto 0) := "11000101";
  constant XCH_A_ATRI    : std_logic_vector(6 downto 0) := "1100011";
  constant XCHD_A_ATRI   : std_logic_vector(6 downto 0) := "1101011";
  constant XRL_A_RR      : std_logic_vector(4 downto 0) := "01101";
  constant XRL_A_D       : std_logic_vector(7 downto 0) := "01100101";
  constant XRL_A_ATRI    : std_logic_vector(6 downto 0) := "0110011";
  constant XRL_A_DATA    : std_logic_vector(7 downto 0) := "01100100";
  constant XRL_D_A       : std_logic_vector(7 downto 0) := "01100010";
  constant XRL_D_DATA    : std_logic_vector(7 downto 0) := "01100011";
  

  type t_state is (STARTUP,
                   FETCH,
                   EXEC1,
                   EXEC2,
                   EXEC3);

  type t_instr_category is (IC_ACALL,
                            IC_ADD_A_RR,
                            IC_ADD_A_D,
                            IC_ADD_A_ATRI,
                            IC_ADD_A_DATA,
                            IC_ADDC_A_RR,
                            IC_ADDC_A_D,
                            IC_ADDC_A_ATRI,
                            IC_ADDC_A_DATA,
                            IC_AJMP,
                            IC_ANL_A_RR,
                            IC_ANL_A_D,
                            IC_ANL_A_ATRI,
                            IC_ANL_A_DATA,
                            IC_ANL_D_A,
                            IC_ANL_D_DATA,
                            IC_ANL_C_BIT,
                            IC_ANL_C_NBIT,
                            IC_CJNE_A_D,
                            IC_CJNE_A_DATA,
                            IC_CJNE_RR_DATA,
                            IC_CJNE_ATRI_DATA,
                            IC_CLR_A,
                            IC_CLR_C,
                            IC_CLR_BIT,
                            IC_CPL_A,
                            IC_CPL_C,
                            IC_CPL_BIT,
                            IC_DA_A,
                            IC_DEC_A,
                            IC_DEC_RR,
                            IC_DEC_D,
                            IC_DEC_ATRI,
                            IC_DIV_AB,
                            IC_DJNZ_RR,
                            IC_DJNZ_D,
                            IC_INC_A,
                            IC_INC_RR,
                            IC_INC_D,
                            IC_INC_ATRI,
                            IC_INC_DPTR,
                            IC_JB,
                            IC_JBC,
                            IC_JC,
                            IC_JMP_A_DPTR,
                            IC_JNB,
                            IC_JNC,
                            IC_JNZ,
                            IC_JZ,
                            IC_LCALL,
                            IC_LJMP,
                            IC_MOV_A_RR,
                            IC_MOV_A_D,
                            IC_MOV_A_ATRI,
                            IC_MOV_A_DATA,
                            IC_MOV_RR_A,
                            IC_MOV_RR_D,
                            IC_MOV_RR_DATA,
                            IC_MOV_D_A,
                            IC_MOV_D_RR,
                            IC_MOV_D_D,
                            IC_MOV_D_ATRI,
                            IC_MOV_D_DATA,
                            IC_MOV_ATRI_A,
                            IC_MOV_ATRI_D,
                            IC_MOV_ATRI_DATA,
                            IC_MOVC_A_ATDPTR,
                            IC_MOVC_A_ATPC,
                            IC_MOVX_A_ATRI,
                            IC_MOVX_A_ATDPTR,
                            IC_MOVX_ATRI_A,
                            IC_MOVX_ATDPTR_A,
                            IC_MOV_C_BIT,
                            IC_MOV_BIT_C,
                            IC_MOV_DPTR_DATA,
                            IC_MUL_AB,
                            IC_NOP,
                            IC_ORL_A_RR,
                            IC_ORL_A_D,
                            IC_ORL_A_ATRI,
                            IC_ORL_A_DATA,
                            IC_ORL_D_A,
                            IC_ORL_D_DATA,
                            IC_ORL_C_BIT,
                            IC_ORL_C_NBIT,
                            IC_POP,
                            IC_PUSH,
                            IC_RET,
                            IC_RETI,
                            IC_RL_A,
                            IC_RLC_A,
                            IC_RR_A,
                            IC_RRC_A,
                            IC_SETB_C,
                            IC_SETB_BIT,
                            IC_SJMP,
                            IC_SUBB_A_RR,
                            IC_SUBB_A_D,
                            IC_SUBB_A_ATRI,
                            IC_SUBB_A_DATA,
                            IC_SWAP_A,
                            IC_XCH_A_RR,
                            IC_XCH_A_D,
                            IC_XCH_A_ATRI,
                            IC_XCHD_A_ATRI,
                            IC_XRL_A_RR,
                            IC_XRL_A_D,
                            IC_XRL_A_ATRI,
                            IC_XRL_A_DATA,
                            IC_XRL_D_A,
                            IC_XRL_D_DATA);
  
  type t_tmr_lv is array(C_IMPL_N_TMR-1 downto 0) of
    std_logic_vector(7 downto 0);
  
  type t_tmr_us is array(C_IMPL_N_TMR-1 downto 0) of unsigned(7 downto 0);
  
  type t_tmr_us2 is array(C_IMPL_N_TMR-1 downto 0) of unsigned(1 downto 0);
  
  type t_tmr_l is array(C_IMPL_N_TMR-1 downto 0) of std_logic;
  
  type t_siu_lv is array(C_IMPL_N_SIU-1 downto 0) of
    std_logic_vector(7 downto 0);
  
  type t_siu_us is array(C_IMPL_N_SIU-1 downto 0) of unsigned(7 downto 0);
  
  type t_siu_l is array(C_IMPL_N_SIU-1 downto 0) of std_logic;
  
  type t_ext_l is array(C_IMPL_N_EXT-1 downto 0) of std_logic;

  
  component addsub_cy
    generic (DWIDTH : integer);
    port (opa_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          opb_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          addsub_i : in  std_logic;
          cy_i     : in  std_logic;
          cy_o     : out std_logic;
          rslt_o   : out std_logic_vector(DWIDTH-1 downto 0));
  end component;

  component addsub_ovcy
    generic (DWIDTH : integer);
    port (opa_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          opb_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          addsub_i : in  std_logic;
          cy_i     : in  std_logic;
          cy_o     : out std_logic;
          ov_o     : out std_logic;
          rslt_o   : out std_logic_vector(DWIDTH-1 downto 0));
  end component;

  component addsub_core
    generic (DWIDTH : integer);
    port (opa_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          opb_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          addsub_i : in  std_logic;
          cy_i     : in  std_logic;
          cy_o     : out std_logic_vector((DWIDTH-1)/4 downto 0);
          ov_o     : out std_logic;
          rslt_o   : out std_logic_vector(DWIDTH-1 downto 0));
  end component;

  component alucore
    generic (DWIDTH : integer);
    port (op_a_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          op_b_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          alu_cmd_i : in  std_logic_vector(3 downto 0);
          cy_i      : in  std_logic_vector((DWIDTH-1)/4 downto 0);
          cy_o      : out std_logic_vector((DWIDTH-1)/4 downto 0);
          result_o  : out std_logic_vector(DWIDTH-1 downto 0));
  end component;

  component comb_divider
    generic (
      DWIDTH : integer);
    port (
      dvdnd_i : in  std_logic_vector(DWIDTH-1 downto 0);
      dvsor_i : in  std_logic_vector(DWIDTH-1 downto 0);
      qutnt_o : out std_logic_vector(DWIDTH-1 downto 0);
      rmndr_o : out std_logic_vector(DWIDTH-1 downto 0));
  end component;

  component comb_mltplr
    generic (
      DWIDTH : integer);
    port (
      mltplcnd_i : in  std_logic_vector(DWIDTH-1 downto 0);
      mltplctr_i : in  std_logic_vector(DWIDTH-1 downto 0);
      product_o  : out std_logic_vector((DWIDTH*2)-1 downto 0));
  end component;

  component dcml_adjust
    generic (
      DWIDTH : integer);
    port (
      data_i : in  std_logic_vector(DWIDTH-1 downto 0);
      cy_i   : in  std_logic_vector((DWIDTH-1)/4 downto 0);
      data_o : out std_logic_vector(DWIDTH-1 downto 0);
      cy_o   : out std_logic);
  end component;

  component alumux
    generic (DWIDTH : integer);
    port (rom_data_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          ram_data_i    : in  std_logic_vector(DWIDTH-1 downto 0);
          acc_i         : in  std_logic_vector(DWIDTH-1 downto 0);
          cmd_i         : in  std_logic_vector(5 downto 0);
          cy_i          : in  std_logic_vector((DWIDTH-1)/4 downto 0);
          ov_i          : in  std_logic;
          cy_o          : out std_logic_vector((DWIDTH-1)/4 downto 0);
          ov_o          : out std_logic;
          result_a_o    : out std_logic_vector(DWIDTH-1 downto 0);
          result_b_o    : out std_logic_vector(DWIDTH-1 downto 0);
          result_i      : in  std_logic_vector(DWIDTH-1 downto 0);
          new_cy_i      : in  std_logic_vector((DWIDTH-1)/4 downto 0);
          addsub_rslt_i : in  std_logic_vector(DWIDTH-1 downto 0);
          addsub_cy_i   : in  std_logic_vector((DWIDTH-1)/4 downto 0);
          addsub_ov_i   : in  std_logic;
          op_a_o        : out std_logic_vector(DWIDTH-1 downto 0);
          op_b_o        : out std_logic_vector(DWIDTH-1 downto 0);
          alu_cmd_o     : out std_logic_vector(3 downto 0);
          opa_o         : out std_logic_vector(DWIDTH-1 downto 0);
          opb_o         : out std_logic_vector(DWIDTH-1 downto 0);
          addsub_o      : out std_logic;
          addsub_cy_o   : out std_logic;
          dvdnd_o       : out std_logic_vector(DWIDTH-1 downto 0);
          dvsor_o       : out std_logic_vector(DWIDTH-1 downto 0);
          qutnt_i       : in  std_logic_vector(DWIDTH-1 downto 0);
          rmndr_i       : in  std_logic_vector(DWIDTH-1 downto 0);
          mltplcnd_o    : out std_logic_vector(DWIDTH-1 downto 0);
          mltplctr_o    : out std_logic_vector(DWIDTH-1 downto 0);
          product_i     : in  std_logic_vector((DWIDTH*2)-1 downto 0);
          dcml_data_o   : out std_logic_vector(DWIDTH-1 downto 0);
          dcml_data_i   : in  std_logic_vector(DWIDTH-1 downto 0);
          dcml_cy_i     : in  std_logic);
  end component;

  component mc8051_alu
    generic (
      DWIDTH : integer);
    port (
      rom_data_i : in  std_logic_vector(DWIDTH-1 downto 0);
      ram_data_i : in  std_logic_vector(DWIDTH-1 downto 0);
      acc_i      : in  std_logic_vector(DWIDTH-1 downto 0);
      cmd_i      : in  std_logic_vector(5 downto 0);
      cy_i       : in  std_logic_vector((DWIDTH-1)/4 downto 0);
      ov_i       : in  std_logic;
      new_cy_o   : out std_logic_vector((DWIDTH-1)/4 downto 0);
      new_ov_o   : out std_logic;
      result_a_o : out std_logic_vector(DWIDTH-1 downto 0);
      result_b_o : out std_logic_vector(DWIDTH-1 downto 0));
  end component;

  component mc8051_siu
    port (clk     : in  std_logic;
          cen     : in  std_logic;
          reset   : in  std_logic;
          tf_i    : in  std_logic;
          trans_i : in  std_logic;
          rxd_i   : in  std_logic;
          scon_i  : in  std_logic_vector(5 downto 0);
          sbuf_i  : in  std_logic_vector(7 downto 0);
          smod_i  : in  std_logic;
          sbuf_o  : out std_logic_vector(7 downto 0);
          scon_o  : out std_logic_vector(2 downto 0);
          rxdwr_o : out std_logic;
          rxd_o   : out std_logic;
          txd_o   : out std_logic);
  end component;


  component mc8051_tmrctr
    port (clk        : in  std_logic;
          cen        : in  std_logic;
          reset      : in  std_logic;
          int0_i     : in  std_logic;
          int1_i     : in  std_logic;
          t0_i       : in  std_logic;
          t1_i       : in  std_logic;
          tmod_i     : in  std_logic_vector(7 downto 0);
          tcon_tr0_i : in  std_logic;
          tcon_tr1_i : in  std_logic;
          reload_i   : in  std_logic_vector(7 downto 0);
          wt_en_i    : in  std_logic;
          wt_i       : in  std_logic_vector(1 downto 0);
          th0_o      : out std_logic_vector(7 downto 0);
          tl0_o      : out std_logic_vector(7 downto 0);
          th1_o      : out std_logic_vector(7 downto 0);
          tl1_o      : out std_logic_vector(7 downto 0);
          tf0_o      : out std_logic;
          tf1_o      : out std_logic);
  end component;


  component control_fsm
    port ( state_i     : in t_state;    -- actual state
            help_i     : in std_logic_vector (7 downto 0);  -- general help-reg
            bit_data_i : in std_logic;  -- bitdata from regs
            aludata_i  : in std_logic_vector (7 downto 0);  -- ALU result
            command_i  : in std_logic_vector (7 downto 0);  -- actual command
            inthigh_i  : in std_logic;  -- high priority int is running
            intlow_i   : in std_logic;  -- low priority int is running
            intpre_i   : in std_logic;  -- an interrupt must start
            intpre2_i  : in std_logic;  -- prepare for interrupt
            intblock_i : in std_logic;  -- interrupt delay at RETI, IE, IP
            ti_i       : in std_logic;
            ri_i       : in std_logic;
            ie0_i      : in std_logic;
            ie1_i      : in std_logic;
            tf0_i      : in std_logic;
            tf1_i      : in std_logic;
            acc        : in std_logic_vector(7 downto 0);
            psw        : in std_logic_vector(7 downto 0);
            ie         : in std_logic_vector(7 downto 0);
            ip         : in std_logic_vector(7 downto 0);

            alu_cmd_o    : out std_logic_vector (5 downto 0);  -- ALU code
            pc_inc_en_o  : out std_logic_vector (3 downto 0);
            nextstate_o  : out t_state;
            adr_mux_o    : out std_logic_vector (3 downto 0);
            adrx_mux_o   : out std_logic_vector (1 downto 0);
            wrx_mux_o    : out std_logic;
            data_mux_o   : out std_logic_vector (3 downto 0);
            bdata_mux_o  : out std_logic_vector (3 downto 0);
            regs_wr_en_o : out std_logic_vector (2 downto 0);
            help_en_o    : out std_logic_vector (3 downto 0);
            help16_en_o  : out std_logic_vector (1 downto 0);
            helpb_en_o   : out std_logic;
            inthigh_en_o : out std_logic;
            intlow_en_o  : out std_logic;
            intpre2_en_o : out std_logic;
            inthigh_d_o  : out std_logic;
            intlow_d_o   : out std_logic;
            intpre2_d_o  : out std_logic;
            ext0isr_d_o   : out std_logic;
            ext1isr_d_o   : out std_logic;
            ext0isrh_d_o  : out std_logic;
            ext1isrh_d_o  : out std_logic;
            ext0isr_en_o  : out std_logic;
            ext1isr_en_o  : out std_logic;
            ext0isrh_en_o : out std_logic;
            ext1isrh_en_o : out std_logic);

  end component;

  component control_mem
    port (pc_o           : out std_logic_vector(15 downto 0);
          rom_data_i     : in  std_logic_vector(7 downto 0);
          ram_data_o     : out std_logic_vector(7 downto 0);
          ram_data_i     : in  std_logic_vector(7 downto 0);
          ram_adr_o      : out std_logic_vector(6 downto 0);
          reg_data_o     : out std_logic_vector(7 downto 0);
          ram_wr_o       : out std_logic;
          cy_o           : out std_logic_vector(1 downto 0);
          ov_o           : out std_logic;
          ram_en_o       : out std_logic;
          aludata_i      : in  std_logic_vector (7 downto 0);
          aludatb_i      : in  std_logic_vector (7 downto 0);
          acc_o          : out std_logic_vector (7 downto 0);
          new_cy_i       : in  std_logic_vector(1 downto 0);
          new_ov_i       : in  std_logic;
          reset          : in  std_logic;
          clk            : in  std_logic;
          cen            : in  std_logic;
          int0_i         : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);
          int1_i         : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);
          p0_i           : in  std_logic_vector(7 downto 0);
          p1_i           : in  std_logic_vector(7 downto 0);
          p2_i           : in  std_logic_vector(7 downto 0);
          p3_i           : in  std_logic_vector(7 downto 0);
          p0_o           : out std_logic_vector(7 downto 0);
          p1_o           : out std_logic_vector(7 downto 0);
          p2_o           : out std_logic_vector(7 downto 0);
          p3_o           : out std_logic_vector(7 downto 0);
          all_trans_o    : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          all_scon_o     : out std_logic_vector(6*C_IMPL_N_SIU-1 downto 0);
          all_sbuf_o     : out std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);
          all_smod_o     : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          all_scon_i     : in  std_logic_vector(3*C_IMPL_N_SIU-1 downto 0);
          all_sbuf_i     : in  std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);
          all_tcon_tr0_o : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tcon_tr1_o : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tmod_o     : out std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_reload_o   : out std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_wt_o       : out std_logic_vector(2*C_IMPL_N_TMR-1 downto 0);
          all_wt_en_o    : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tf0_i      : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tf1_i      : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tl0_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_tl1_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_th0_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_th1_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          state_o        : out t_state;
          help_o         : out std_logic_vector(7 downto 0);
          bit_data_o     : out std_logic;
          command_o      : out std_logic_vector (7 downto 0);
          inthigh_o      : out std_logic;
          intlow_o       : out std_logic;
          intpre_o       : out std_logic;
          intpre2_o      : out std_logic;
          intblock_o     : out std_logic;
          ti_o           : out std_logic;
          ri_o           : out std_logic;
          ie0_o          : out std_logic;
          ie1_o          : out std_logic;
          tf0_o          : out std_logic;
          tf1_o          : out std_logic;
          psw_o          : out std_logic_vector(7 downto 0);
          ie_o           : out std_logic_vector(7 downto 0);
          ip_o           : out std_logic_vector(7 downto 0);
          adrx_o         : out std_logic_vector(15 downto 0);
          datax_o        : out std_logic_vector(7 downto 0);
          memx_o         : out std_logic;
          wrx_o          : out std_logic;
          datax_i        : in  std_logic_vector(7 downto 0);
          pc_inc_en_i    : in  std_logic_vector (3 downto 0);
          nextstate_i    : in  t_state;
          adr_mux_i      : in  std_logic_vector (3 downto 0);
          adrx_mux_i     : in  std_logic_vector (1 downto 0);
          wrx_mux_i      : in  std_logic;
          data_mux_i     : in  std_logic_vector (3 downto 0);
          bdata_mux_i    : in  std_logic_vector (3 downto 0);
          regs_wr_en_i   : in  std_logic_vector (2 downto 0);
          help_en_i      : in  std_logic_vector (3 downto 0);
          help16_en_i    : in  std_logic_vector (1 downto 0);
          helpb_en_i     : in  std_logic;
          inthigh_en_i   : in  std_logic;
          intlow_en_i    : in  std_logic;
          intpre2_en_i   : in  std_logic;
          inthigh_d_i    : in  std_logic;
          intlow_d_i     : in  std_logic;
          intpre2_d_i    : in  std_logic;
          ext0isr_d_i    : in std_logic;
          ext1isr_d_i    : in std_logic;
          ext0isrh_d_i   : in std_logic;
          ext1isrh_d_i   : in std_logic;
          ext0isr_en_i   : in std_logic;
          ext1isr_en_i   : in std_logic;
          ext0isrh_en_i  : in std_logic;
          ext1isrh_en_i  : in std_logic);

  end component;

  component mc8051_control
    port (pc_o           : out std_logic_vector(15 downto 0);
          rom_data_i     : in  std_logic_vector(7 downto 0);
          ram_data_o     : out std_logic_vector(7 downto 0);
          ram_data_i     : in  std_logic_vector(7 downto 0);
          ram_adr_o      : out std_logic_vector(6 downto 0);
          reg_data_o     : out std_logic_vector(7 downto 0);
          ram_wr_o       : out std_logic;
          cy_o           : out std_logic_vector(1 downto 0);
          ov_o           : out std_logic;
          ram_en_o       : out std_logic;
          alu_cmd_o      : out std_logic_vector (5 downto 0);
          aludata_i      : in  std_logic_vector (7 downto 0);
          aludatb_i      : in  std_logic_vector (7 downto 0);
          acc_o          : out std_logic_vector (7 downto 0);
          new_cy_i       : in  std_logic_vector(1 downto 0);
          new_ov_i       : in  std_logic;
          reset          : in  std_logic;
          clk            : in  std_logic;
          cen            : in  std_logic;
          int0_i         : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);
          int1_i         : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);
          datax_i        : in  std_logic_vector (7 downto 0);
          datax_o        : out std_logic_vector (7 downto 0);
          adrx_o         : out std_logic_vector (15 downto 0);
          memx_o         : out std_logic;
          wrx_o          : out std_logic;
          p0_i           : in  std_logic_vector(7 downto 0);
          p1_i           : in  std_logic_vector(7 downto 0);
          p2_i           : in  std_logic_vector(7 downto 0);
          p3_i           : in  std_logic_vector(7 downto 0);
          p0_o           : out std_logic_vector(7 downto 0);
          p1_o           : out std_logic_vector(7 downto 0);
          p2_o           : out std_logic_vector(7 downto 0);
          p3_o           : out std_logic_vector(7 downto 0);
          all_trans_o    : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          all_scon_o     : out std_logic_vector(6*C_IMPL_N_SIU-1 downto 0);
          all_sbuf_o     : out std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);
          all_smod_o     : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          all_scon_i     : in  std_logic_vector(3*C_IMPL_N_SIU-1 downto 0);
          all_sbuf_i     : in  std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);
          all_tcon_tr0_o : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tcon_tr1_o : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tmod_o     : out std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_reload_o   : out std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_wt_o       : out std_logic_vector(2*C_IMPL_N_TMR-1 downto 0);
          all_wt_en_o    : out std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tf0_i      : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tf1_i      : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_tl0_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_tl1_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_th0_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
          all_th1_i      : in  std_logic_vector(8*C_IMPL_N_TMR-1 downto 0));

  end component;


  component mc8051_core
    port (clk         : in  std_logic;
          cen         : in  std_logic;
          reset       : in  std_logic;
          rom_data_i  : in  std_logic_vector(7 downto 0);
          ram_data_i  : in  std_logic_vector(7 downto 0);
          int0_i      : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);
          int1_i      : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);
          all_t0_i    : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_t1_i    : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_rxd_i   : in  std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          p0_i        : in  std_logic_vector(7 downto 0);
          p1_i        : in  std_logic_vector(7 downto 0);
          p2_i        : in  std_logic_vector(7 downto 0);
          p3_i        : in  std_logic_vector(7 downto 0);
          p0_o        : out std_logic_vector(7 downto 0);
          p1_o        : out std_logic_vector(7 downto 0);
          p2_o        : out std_logic_vector(7 downto 0);
          p3_o        : out std_logic_vector(7 downto 0);
          all_rxd_o   : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          all_txd_o   : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          all_rxdwr_o : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          rom_adr_o   : out std_logic_vector(15 downto 0);
          ram_data_o  : out std_logic_vector(7 downto 0);
          ram_adr_o   : out std_logic_vector(6 downto 0);
          ram_wr_o    : out std_logic;
          ram_en_o    : out std_logic;
          datax_i     : in  std_logic_vector (7 downto 0);
          datax_o     : out std_logic_vector (7 downto 0);
          adrx_o      : out std_logic_vector (15 downto 0);
          memx_o      : out std_logic;
          wrx_o       : out std_logic);

  end component;


  component mc8051_top
    port (clk         : in  std_logic;
          cen         : in  std_logic;
          reset       : in  std_logic;
          int0_i      : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);
          int1_i      : in  std_logic_vector(C_IMPL_N_EXT-1 downto 0);
          all_t0_i    : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_t1_i    : in  std_logic_vector(C_IMPL_N_TMR-1 downto 0);
          all_rxd_i   : in  std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          p0_i        : in  std_logic_vector(7 downto 0);
          p1_i        : in  std_logic_vector(7 downto 0);
          p2_i        : in  std_logic_vector(7 downto 0);
          p3_i        : in  std_logic_vector(7 downto 0);
          p0_o        : out std_logic_vector(7 downto 0);
          p1_o        : out std_logic_vector(7 downto 0);
          p2_o        : out std_logic_vector(7 downto 0);
          p3_o        : out std_logic_vector(7 downto 0);
          all_rxd_o   : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          all_txd_o   : out std_logic_vector(C_IMPL_N_SIU-1 downto 0);
          all_rxdwr_o : out std_logic_vector(C_IMPL_N_SIU-1 downto 0));  

  end component;

  -----------------------------------------------------------------------------
  -- START: Component declarations for simulation models
  -----------------------------------------------------------------------------
  component mc8051_ram
    port (clk        : in  std_logic;
          cen        : in  std_logic;
          reset      : in  std_logic;
          ram_data_i : in  std_logic_vector(7 downto 0);
          ram_data_o : out std_logic_vector(7 downto 0);
          ram_adr_i  : in  std_logic_vector(6 downto 0);
          ram_wr_i   : in  std_logic;
          ram_en_i   : in  std_logic);

  end component;

  component mc8051_ramx
    port (clk        : in  std_logic;
          cen        : in  std_logic;
          reset      : in  std_logic;
          ram_data_i : in  std_logic_vector(7 downto 0);
          ram_data_o : out std_logic_vector(7 downto 0);
          ram_adr_i  : in  std_logic_vector(15 downto 0);
          ram_wr_i   : in  std_logic);

  end component;

  component mc8051_rom
    port (clk        : in  std_logic;
          cen        : in  std_logic;
          reset      : in  std_logic;
          rom_data_o : out std_logic_vector(7 downto 0); 
          rom_adr_i  : in  std_logic_vector(15 downto 0));
    
  end component;
  -----------------------------------------------------------------------------
  -- END: Component declarations for simulation models
  -----------------------------------------------------------------------------


end mc8051_p;
