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
--         Author:                 Helmut Mayrhofer
--
--         Filename:               control_fsm_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.10 $
--
--         Date of Latest Version: $Date: 2010-03-24 10:20:48 $
--
--
--         Description: Decode instruction and execute it. Pure combinational
--                      descripton of the finite state machine.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of control_fsm is

  alias CY : std_logic is psw(7);              -- carry
  alias AC : std_logic is psw(6);              -- auxilary carry
  alias OV : std_logic is psw(2);              -- overflow flag
  alias EA:  std_logic is ie(7);               -- enable interupts
  alias ES:  std_logic is ie(4);               -- enable serial interupt
  alias ET1: std_logic is ie(3);               -- enable timer 1 interupt
  alias EX1: std_logic is ie(2);               -- enable external 1 interupt
  alias ET0: std_logic is ie(1);               -- enable timer 0 interupt
  alias EX0: std_logic is ie(0);               -- enable external 0 interupt
  alias PS0: std_logic is ip(4);               -- priority of serial interupt
  alias PT1: std_logic is ip(3);               -- priority of timer1 interupt
  alias PX1: std_logic is ip(2);               -- priority of ext1 interupt
  alias PT0: std_logic is ip(1);               -- priority of timer0 interupt
  alias PX0: std_logic is ip(0);               -- priority of ext0 interupt

  signal state:         t_state;               -- actual state
  signal s_nextstate:   t_state;               -- enable signal for state
  signal s_instr_category : t_instr_category;  -- predecoded insturction
  
  signal s_help:        unsigned (7 downto 0); -- general help-register

  signal s_bit_data :   std_logic;             -- bitdata from MUX
  signal s_intpre:      std_logic;             -- an interrupt must start
  signal s_intpre2:     std_logic;             -- prepare for interrupt
  signal s_inthigh:     std_logic;             -- high priority int is running
  signal s_intlow:      std_logic;             -- low priority int is running

  signal s_tf1 :        std_logic;               -- Timer1-Overflowflag
  signal s_tf0 :        std_logic;               -- Timer0-Overflowflag
  signal s_ie1 :        std_logic;               -- ExtINT1-Flag
  signal s_ie0 :        std_logic;               -- ExtINT0-Flag

  signal s_ri : std_logic;                       -- serial receive-ready
  signal s_ti : std_logic;                       -- serial transmit-ready

  signal s_command:     std_logic_vector (7 downto 0); -- actual command

  signal s_pc_inc_en  : std_logic_vector (3 downto 0);  -- pc control
  signal s_regs_wr_en : std_logic_vector (2 downto 0);  -- write control
  signal s_data_mux   : std_logic_vector (3 downto 0);  -- data control
  signal s_bdata_mux  : std_logic_vector (3 downto 0);  -- bitdata control
  signal s_adr_mux    : std_logic_vector (3 downto 0);  -- adress control
  signal s_adrx_mux   : std_logic_vector (1 downto 0);  -- ext. adress control
  signal s_wrx_mux    : std_logic;                      -- ext. write control
  signal s_help_en    : std_logic_vector (3 downto 0);  -- helpreg control
  signal s_help16_en  : std_logic_vector (1 downto 0);  -- help16 control
  signal s_helpb_en   : std_logic;                      -- helpbit control
  signal s_intpre2_d  : std_logic;                      -- intput of intpre2
  signal s_intpre2_en : std_logic;                      -- control
  signal s_intlow_d   : std_logic;                      -- input of intlow
  signal s_intlow_en  : std_logic;                      -- control
  signal s_inthigh_d  : std_logic;                      -- input of inthigh
  signal s_inthigh_en : std_logic;                      -- control
  signal s_ext0isr_d  : std_logic;      -- ext int 0 ISR running if set
  signal s_ext1isr_d  : std_logic;      -- ext int 1 ISR running if set
  signal s_ext0isrh_d : std_logic;      -- ext int 0 ISR has high priority if 1
  signal s_ext1isrh_d : std_logic;      -- ext int 1 ISR has high priority if 1
  signal s_ext0isr_en  : std_logic;
  signal s_ext1isr_en  : std_logic;
  signal s_ext0isrh_en : std_logic;
  signal s_ext1isrh_en : std_logic;

begin

  -- some simple signal-assignments for outputs

  pc_inc_en_o  <= s_pc_inc_en;
  nextstate_o  <= s_nextstate;
  adr_mux_o    <= s_adr_mux;
  adrx_mux_o   <= s_adrx_mux;
  wrx_mux_o    <= s_wrx_mux;
  data_mux_o   <= s_data_mux;
  bdata_mux_o  <= s_bdata_mux;
  regs_wr_en_o <= s_regs_wr_en;
  help_en_o    <= s_help_en;
  help16_en_o  <= s_help16_en;
  helpb_en_o   <= s_helpb_en;
  inthigh_en_o <= s_inthigh_en;
  intlow_en_o  <= s_intlow_en;
  intpre2_en_o <= s_intpre2_en;
  inthigh_d_o  <= s_inthigh_d;
  intlow_d_o   <= s_intlow_d;
  intpre2_d_o  <= s_intpre2_d;
  ext0isr_d_o   <= s_ext0isr_d;
  ext1isr_d_o   <= s_ext1isr_d;
  ext0isrh_d_o  <= s_ext0isrh_d;
  ext1isrh_d_o  <= s_ext1isrh_d;
  ext0isr_en_o  <= s_ext0isr_en;
  ext1isr_en_o  <= s_ext1isr_en;
  ext0isrh_en_o <= s_ext0isrh_en;
  ext1isrh_en_o <= s_ext1isrh_en;

  -- some simple signal assignments from intputs

  state      <= state_i;
  s_help     <= unsigned(help_i);
  s_bit_data <= bit_data_i;
  s_command  <= command_i;
  s_inthigh  <= inthigh_i;
  s_intlow   <= intlow_i;
  s_intpre   <= intpre_i;
  s_intpre2  <= intpre2_i;
  s_ti       <= ti_i;
  s_ri       <= ri_i;
  s_ie0      <= ie0_i;
  s_ie1      <= ie1_i;
  s_tf0      <= tf0_i;
  s_tf1      <= tf1_i;

  -- predecode instruction
  s_instr_category <=
    IC_ACALL          when s_command(4 downto 0) = ACALL          else
    IC_ADD_A_RR       when s_command(7 downto 3) = ADD_A_RR       else
    IC_ADD_A_D        when s_command             = ADD_A_D        else
    IC_ADD_A_ATRI     when s_command(7 downto 1) = ADD_A_ATRI     else
    IC_ADD_A_DATA     when s_command             = ADD_A_DATA     else
    IC_ADDC_A_RR      when s_command(7 downto 3) = ADDC_A_RR      else
    IC_ADDC_A_D       when s_command             = ADDC_A_D       else
    IC_ADDC_A_ATRI    when s_command(7 downto 1) = ADDC_A_ATRI    else
    IC_ADDC_A_DATA    when s_command             = ADDC_A_DATA    else
    IC_AJMP           when s_command(4 downto 0) = AJMP           else
    IC_ANL_A_RR       when s_command(7 downto 3) = ANL_A_RR       else
    IC_ANL_A_D        when s_command             = ANL_A_D        else
    IC_ANL_A_ATRI     when s_command(7 downto 1) = ANL_A_ATRI     else
    IC_ANL_A_DATA     when s_command             = ANL_A_DATA     else
    IC_ANL_D_A        when s_command             = ANL_D_A        else
    IC_ANL_D_DATA     when s_command             = ANL_D_DATA     else
    IC_ANL_C_BIT      when s_command             = ANL_C_BIT      else
    IC_ANL_C_NBIT     when s_command             = ANL_C_NBIT     else
    IC_CJNE_A_D       when s_command             = CJNE_A_D       else
    IC_CJNE_A_DATA    when s_command             = CJNE_A_DATA    else
    IC_CJNE_RR_DATA   when s_command(7 downto 3) = CJNE_RR_DATA   else
    IC_CJNE_ATRI_DATA when s_command(7 downto 1) = CJNE_ATRI_DATA else
    IC_CLR_A          when s_command             = CLR_A          else
    IC_CLR_C          when s_command             = CLR_C          else
    IC_CLR_BIT        when s_command             = CLR_BIT        else
    IC_CPL_A          when s_command             = CPL_A          else
    IC_CPL_C          when s_command             = CPL_C          else
    IC_CPL_BIT        when s_command             = CPL_BIT        else
    IC_DA_A           when s_command             = DA_A           else
    IC_DEC_A          when s_command             = DEC_A          else
    IC_DEC_RR         when s_command(7 downto 3) = DEC_RR         else
    IC_DEC_D          when s_command             = DEC_D          else
    IC_DEC_ATRI       when s_command(7 downto 1) = DEC_ATRI       else
    IC_DIV_AB         when s_command             = DIV_AB         else
    IC_DJNZ_RR        when s_command(7 downto 3) = DJNZ_RR        else
    IC_DJNZ_D         when s_command             = DJNZ_D         else
    IC_INC_A          when s_command             = INC_A          else
    IC_INC_RR         when s_command(7 downto 3) = INC_RR         else
    IC_INC_D          when s_command             = INC_D          else
    IC_INC_ATRI       when s_command(7 downto 1) = INC_ATRI       else
    IC_INC_DPTR       when s_command             = INC_DPTR       else
    IC_JB             when s_command             = JB             else
    IC_JBC            when s_command             = JBC            else
    IC_JC             when s_command             = JC             else
    IC_JMP_A_DPTR     when s_command             = JMP_A_DPTR     else
    IC_JNB            when s_command             = JNB            else
    IC_JNC            when s_command             = JNC            else
    IC_JNZ            when s_command             = JNZ            else
    IC_JZ             when s_command             = JZ             else
    IC_LCALL          when s_command             = LCALL          else
    IC_LJMP           when s_command             = LJMP           else
    IC_MOV_A_RR       when s_command(7 downto 3) = MOV_A_RR       else
    IC_MOV_A_D        when s_command             = MOV_A_D        else
    IC_MOV_A_ATRI     when s_command(7 downto 1) = MOV_A_ATRI     else
    IC_MOV_A_DATA     when s_command             = MOV_A_DATA     else
    IC_MOV_RR_A       when s_command(7 downto 3) = MOV_RR_A       else
    IC_MOV_RR_D       when s_command(7 downto 3) = MOV_RR_D       else
    IC_MOV_RR_DATA    when s_command(7 downto 3) = MOV_RR_DATA    else
    IC_MOV_D_A        when s_command             = MOV_D_A        else
    IC_MOV_D_RR       when s_command(7 downto 3) = MOV_D_RR       else
    IC_MOV_D_D        when s_command             = MOV_D_D        else
    IC_MOV_D_ATRI     when s_command(7 downto 1) = MOV_D_ATRI     else
    IC_MOV_D_DATA     when s_command             = MOV_D_DATA     else
    IC_MOV_ATRI_A     when s_command(7 downto 1) = MOV_ATRI_A     else
    IC_MOV_ATRI_D     when s_command(7 downto 1) = MOV_ATRI_D     else
    IC_MOV_ATRI_DATA  when s_command(7 downto 1) = MOV_ATRI_DATA  else
    IC_MOVC_A_ATDPTR  when s_command             = MOVC_A_ATDPTR  else
    IC_MOVC_A_ATPC    when s_command             = MOVC_A_ATPC    else
    IC_MOVX_A_ATRI    when s_command(7 downto 1) = MOVX_A_ATRI    else
    IC_MOVX_A_ATDPTR  when s_command             = MOVX_A_ATDPTR  else
    IC_MOVX_ATRI_A    when s_command(7 downto 1) = MOVX_ATRI_A    else
    IC_MOVX_ATDPTR_A  when s_command             = MOVX_ATDPTR_A  else
    IC_MOV_C_BIT      when s_command             = MOV_C_BIT      else
    IC_MOV_BIT_C      when s_command             = MOV_BIT_C      else
    IC_MOV_DPTR_DATA  when s_command             = MOV_DPTR_DATA  else
    IC_MUL_AB         when s_command             = MUL_AB         else
    IC_NOP            when s_command             = NOP            else
    IC_ORL_A_RR       when s_command(7 downto 3) = ORL_A_RR       else
    IC_ORL_A_D        when s_command             = ORL_A_D        else
    IC_ORL_A_ATRI     when s_command(7 downto 1) = ORL_A_ATRI     else
    IC_ORL_A_DATA     when s_command             = ORL_A_DATA     else
    IC_ORL_D_A        when s_command             = ORL_D_A        else
    IC_ORL_D_DATA     when s_command             = ORL_D_DATA     else
    IC_ORL_C_BIT      when s_command             = ORL_C_BIT      else
    IC_ORL_C_NBIT     when s_command             = ORL_C_NBIT     else
    IC_POP            when s_command             = POP            else
    IC_PUSH           when s_command             = PUSH           else
    IC_RET            when s_command             = RET            else
    IC_RETI           when s_command             = RETI           else
    IC_RL_A           when s_command             = RL_A           else
    IC_RLC_A          when s_command             = RLC_A          else
    IC_RR_A           when s_command             = RR_A           else
    IC_RRC_A          when s_command             = RRC_A          else
    IC_SETB_C         when s_command             = SETB_C         else
    IC_SETB_BIT       when s_command             = SETB_BIT       else
    IC_SJMP           when s_command             = SJMP           else
    IC_SUBB_A_RR      when s_command(7 downto 3) = SUBB_A_RR      else
    IC_SUBB_A_D       when s_command             = SUBB_A_D       else
    IC_SUBB_A_ATRI    when s_command(7 downto 1) = SUBB_A_ATRI    else
    IC_SUBB_A_DATA    when s_command             = SUBB_A_DATA    else
    IC_SWAP_A         when s_command             = SWAP_A         else
    IC_XCH_A_RR       when s_command(7 downto 3) = XCH_A_RR       else
    IC_XCH_A_D        when s_command             = XCH_A_D        else
    IC_XCH_A_ATRI     when s_command(7 downto 1) = XCH_A_ATRI     else
    IC_XCHD_A_ATRI    when s_command(7 downto 1) = XCHD_A_ATRI    else
    IC_XRL_A_RR       when s_command(7 downto 3) = XRL_A_RR       else
    IC_XRL_A_D        when s_command             = XRL_A_D        else
    IC_XRL_A_ATRI     when s_command(7 downto 1) = XRL_A_ATRI     else
    IC_XRL_A_DATA     when s_command             = XRL_A_DATA     else
    IC_XRL_D_A        when s_command             = XRL_D_A        else
    IC_XRL_D_DATA     when s_command             = XRL_D_DATA     else
    IC_NOP;
  
------------------------------------------------------------------------------
-- purpose: main-process to control the mcu
--          includes the interupt-handling and the state machine
-- inputs:  state,s_help,s_ti,s_ri,s_ie0,s_ie1,s_tf0,s_tf1,
--          s_bit_data,aludata_i,s_command,s_inthigh,
--          s_intlow,acc,psw,s_intpre,s_intpre2,ie,ip
-- outputs: pc_inc_en_o,s_nextstate_o,adr_mux_o,data_mux_o,bdata_mux_o
--          regs_wr_en_o,help_en_o,help16_en_o,helpb_en_o,inthigh_en_o
--          intlow_en_o,intpre2_en_o,inthigh_d_o,intlow_d_o,intpre2_d_o
------------------------------------------------------------------------------

 p_state: process (state,s_help,s_ti,s_ri,s_ie0,s_ie1,s_tf0,s_tf1,
                   s_bit_data,aludata_i,s_instr_category,s_inthigh,
                   s_intlow,acc,psw,s_intpre,s_intpre2,ie,ip,intblock_i)
  begin
    s_data_mux    <= "0000";            -- default values
    s_adr_mux     <= "0000";
    s_bdata_mux   <= "0000";
    alu_cmd_o     <= OFF;
    s_regs_wr_en  <= "000";
    s_help_en     <= "0000";
    s_help16_en   <= "00";
    s_helpb_en    <= '0';
    s_pc_inc_en   <= "0000";
    s_inthigh_en  <= '0';
    s_intlow_en   <= '0';
    s_intpre2_en  <= '0';
    s_inthigh_d   <= '0';
    s_intlow_d    <= '0';
    s_intpre2_d   <= '0';
    s_adrx_mux    <= "00";
    s_wrx_mux     <= '0';
    s_nextstate   <= FETCH;
    s_ext0isr_d   <= '0';
    s_ext1isr_d   <= '0';
    s_ext0isrh_d  <= '0';
    s_ext1isrh_d  <= '0';
    s_ext0isr_en  <= '0';
    s_ext1isr_en  <= '0';
    s_ext0isrh_en <= '0';
    s_ext1isrh_en <= '0';

    if state=STARTUP then
      -- Power up wait cycle
      NULL;
    else
      -- begin of starting the interrupt procedure
                                                      -- saving the old adress
      if intblock_i = '0' and s_instr_category /= IC_RETI and -- fixes bug when
        -- a higher priority interrupt occurred during the RETI of a lower priority one
        -- by wickerwaka https://github.com/MiSTer-devel/Arcade-IremM72_MiSTer/commit/2218dc0
         ((s_intpre='1' and state=FETCH) or s_intpre2='1') then
        if state=FETCH then
          s_intpre2_en <= '1';
          s_intpre2_d <= '1';
          s_regs_wr_en <= "001";                -- increment stackpointer
          s_nextstate <= EXEC1;
        elsif state=EXEC1 then
          if (PX0 and EX0 and s_ie0)='1' then   -- external interrupt 0
            s_help_en <= "0101";                -- interruptvector 0003h
            s_inthigh_d <= '1';
            s_inthigh_en <= '1';
            s_adr_mux <= "0001";
            s_regs_wr_en <= "110";                -- reset IE0
            s_ext0isrh_d  <= '1';
            s_ext0isrh_en <= '1';
          elsif (PT0 and ET0 and s_tf0)='1' then  -- timer interrupt 0
            s_help_en <= "0110";                  -- interruptvector 000Bh
            s_inthigh_d <= '1';
            s_inthigh_en <= '1';
            s_adr_mux <= "0010";
            s_regs_wr_en <= "110";                -- reset TF0
          elsif (PX1 and EX1 and s_ie1)='1' then  -- external interrupt 1
            s_help_en <= "0111";                  -- interruptvector 0013h
            s_inthigh_d <= '1';
            s_inthigh_en <= '1';
            s_adr_mux <= "0011";
            s_regs_wr_en <= "110";                -- reset IE1
            s_ext1isrh_d  <= '1';
            s_ext1isrh_en <= '1';
          elsif (PT1 and ET1 and s_tf1)='1' then  -- timer interrupt 1
            s_help_en <= "1000";                  -- interruptvector 001Bh
            s_inthigh_d <= '1';
            s_inthigh_en <= '1';
            s_adr_mux <= "0100";
            s_regs_wr_en <= "110";          -- reset TF1
          elsif (PS0 and ES and (s_ri or s_ti))='1' then -- serial interrupt 0
            s_help_en <= "1001";            -- interruptvector 0023h
            s_inthigh_d <= '1';
            s_inthigh_en <= '1';
         elsif (EX0 and s_ie0)='1' then   -- external interrupt 0 low priority
            s_help_en <= "0101";          -- interruptvector 0003h
            s_intlow_d <= '1';
            s_intlow_en <= '1';
            s_adr_mux <= "0001";
            s_regs_wr_en <= "110";         -- reset IE0
            s_ext0isr_d   <= '1';
            s_ext0isr_en  <= '1';
          elsif (ET0 and s_tf0)='1' then   -- timer interrupt 0 low priority
            s_help_en <= "0110";           -- interruptvector 000Bh
            s_intlow_d <= '1';
            s_intlow_en <= '1';
            s_adr_mux <= "0010";
            s_regs_wr_en <= "110";         -- reset TF0
          elsif (EX1 and s_ie1)='1' then   -- external interrupt 1 low priority
            s_help_en <= "0111";           -- interruptvector 0013h
            s_intlow_d <= '1';
            s_intlow_en <= '1';
            s_adr_mux <= "0011";
            s_regs_wr_en <= "110";        -- reset IE1
            s_ext1isr_d   <= '1';
            s_ext1isr_en  <= '1';
          elsif (ET1 and s_tf1)='1' then   -- timer interrupt 1 low priority
            s_help_en <= "1000";           -- interruptvector 001Bh
            s_intlow_d <= '1';
            s_intlow_en <= '1';
            s_adr_mux <= "0100";
            s_regs_wr_en <= "110";               -- reset TF1
          elsif (ES and (s_ri or s_ti))='1' then -- serial int 0 low priority
            s_help_en <= "1001";                 -- interruptvector 0023h
            s_intlow_d <= '1';
            s_intlow_en <= '1';
          else
            NULL;
          end if;
          s_nextstate <= EXEC2;
        elsif state=EXEC2 then
          s_adr_mux <= "0101";
          s_data_mux <= "0001";         -- data = pc(7 downto 0)
          s_regs_wr_en <= "101";        -- write one byte and increment SP
          s_nextstate <= EXEC3;
        elsif state=EXEC3 then
          s_intpre2_d <= '0';
          s_intpre2_en <= '1';
          s_adr_mux <= "0101";
          s_data_mux <= "0010";         -- data = pc(15 downto 8)
          s_regs_wr_en <= "100";        -- write one byte
          s_pc_inc_en <= "0011";        -- load program counter
          s_nextstate <= FETCH;
        else
          s_nextstate <= FETCH;
        end if;

       -- end of starting interrupt procedure

       else

        case s_instr_category is

          ---------------------------------------------------------------------

          when IC_ACALL =>              -- ACALL addr11
            if state=FETCH then
              s_adr_mux <= "1111";      -- adress = sp + 1
              s_data_mux <= "1110";     -- data = (pc+2)(7 downto 0)
              s_regs_wr_en <= "101";    -- write one byte and increment SP
              s_help16_en <= "10";      -- s_help16 = pc+2
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1111";       -- adress = sp + 1
              s_data_mux <= "1101";      -- data = s_help16(15 downto 8)
              s_regs_wr_en <= "101";     -- write one byte and increment SP
              s_pc_inc_en <= "0100";     -- load PC with 11 bits (2k block)
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ADD_A_RR =>           -- ADD A,Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- adress = RR-adress
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= ADD_ACC_RAM; -- addition command (ACC + RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ADD_A_D =>            -- ADD A, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- adress = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= ADD_ACC_RAM; -- addition command (ACC + RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ADD_A_ATRI =>         -- ADD A,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= ADD_ACC_RAM; -- addition command (ACC + RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ADD_A_DATA =>         -- ADD A, DATA
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= ADD_ACC_ROM; -- addition command (ACC + ROM_DATA_I)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ADDC_A_RR =>          -- ADDC A,Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- adress = RR-adress
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= ADDC_ACC_RAM;  -- addition command (ACC+RAM_DATA+CY)
              s_data_mux <= "0011";       -- data = aludata_i
              s_regs_wr_en <= "011";      -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";      -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ADDC_A_D =>           -- ADDC A, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- adress = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= ADDC_ACC_RAM;  -- addition command (ACC+RAM_DATA+CY)
              s_data_mux <= "0011";       -- data = aludata_i
              s_regs_wr_en <= "011";      -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";      -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ADDC_A_ATRI =>        -- ADDC A,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= ADDC_ACC_RAM;  -- addition command (ACC+RAM_DATA+CY)
              s_data_mux <= "0011";       -- data = aludata_i
              s_regs_wr_en <= "011";      -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";      -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ADDC_A_DATA =>        -- ADDC A, data
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= ADDC_ACC_ROM;-- addition command (ACC+ROM_DATA_I+CY)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_AJMP =>               -- AJMP addr11
            if state=FETCH then
              s_help16_en <= "10";      -- s_help16 = pc+2
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_pc_inc_en <= "0100";    -- load PC with 11 bits (2k block)
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ANL_A_RR =>           -- ANL A,Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- adress = RR-adress
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= AND_ACC_RAM; -- AND command (ACC ^ RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ANL_A_D =>            -- ANL A, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= AND_ACC_RAM; -- AND command (ACC ^ RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ANL_A_ATRI =>          -- ANL A,ATRi
            if state=FETCH then
              s_adr_mux <= "0111";       -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= AND_ACC_RAM;  -- AND command (ACC ^ RAM_DATA)
              s_data_mux <= "0011";      -- data = aludata_i
              s_regs_wr_en <= "010";     -- write ACC
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_ANL_A_DATA =>          -- ANL A, data
            if state=FETCH then
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= AND_ACC_ROM;   -- AND command (ACC ^ ROM_DATA_I)
              s_data_mux <= "0011";       -- data = aludata_i
              s_regs_wr_en <= "010";      -- write ACC
              s_pc_inc_en <= "0001";      -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ANL_D_A =>            -- ANL direct, A
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- adress = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= AND_ACC_RAM;  -- AND command (ACC ^ RAM_DATA)
              s_adr_mux <= "1000";       -- adress = rom_data_i
              s_data_mux <= "0011";      -- data = aludata_i
              s_regs_wr_en <= "100";     -- write one byte
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ANL_D_DATA =>         -- ANL direct, DATA
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0001";      -- help = rom_data_i
              s_adr_mux <= "1000";      -- adress = rom_data_i
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= AND_RAM_ROM;  -- AND command (ROM_DATA_I ^ RAM_DATA)
              s_adr_mux <= "1010";       -- adress = help
              s_data_mux <= "0011";      -- data = aludata_i
              s_regs_wr_en <= "100";     -- write one byte
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ANL_C_BIT =>          -- ANL C, bit
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- adress = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_bdata_mux <= "0001";    -- bdata = s_bit_data and cy
              s_regs_wr_en <= "110";    -- write one bit (automatic CY-address)
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ANL_C_NBIT =>         -- ANL C, /bit
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- adress = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_bdata_mux <= "0010";    -- bdata = not (s_bit_data and cy)
              s_regs_wr_en <= "110";    -- write one bit (automatic CY-address)
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_CJNE_A_D =>           -- CJNE A, direct, rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= COMP_RAM_ACC;  -- Compare RAM_DATA/ACC operation
              if unsigned(aludata_i) /= 0 then
                s_adr_mux <= "1011";      -- adress of CY
                s_bdata_mux <= "0011";    -- bdata = cby_i
                s_regs_wr_en <= "110";    -- write one bit
                s_pc_inc_en <= "0010";    -- add relativ adress to PC
              else
                     s_adr_mux <= "1011";        -- adress of CY      (CV: changed V1.5 according to specification: clear carry)
                     s_bdata_mux <= "0000";      -- bdata = 0    
                     s_regs_wr_en <= "110";      -- write one bit     
                s_pc_inc_en <= "0001";    -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_CJNE_A_DATA =>        -- CJNE A, #data, rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= COMP_ROM_ACC; -- Compare ROM_DATA_I/ACC operation
              s_help_en <= "0010";
              s_helpb_en <= '1';         -- save new_cy_i
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              if s_help /= 0 then
                s_adr_mux <= "1011";     -- adress of CY
                s_bdata_mux <= "0100";   -- bdata = s_helpb
                s_regs_wr_en <= "110";   -- write one bit
                s_pc_inc_en <= "0010";   -- add relativ adress to PC
              else
                     s_adr_mux <= "1011";        -- adress of CY      (CV: changed V1.5 according to specification: clear carry)
                     s_bdata_mux <= "0000";      -- bdata = 0    
                     s_regs_wr_en <= "110";      -- write one bit     
                s_pc_inc_en <= "0001";   -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_CJNE_RR_DATA =>        -- CJNE RR, #data, rel
            if state=FETCH then
              s_adr_mux <= "0110";       -- address = rr-address
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= COMP_ROM_RAM;-- Compare ROM_DATA_I/RAM_DATA operat.
              s_help_en <= "0010";      -- save aludata_i
              s_helpb_en <= '1';        -- save new_cy_i
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              if s_help /= 0 then
                s_adr_mux <= "1011";     -- adress of CY
                s_bdata_mux <= "0100";   -- bdata = s_helpb
                s_regs_wr_en <= "110";   -- write one bit
                s_pc_inc_en <= "0010";   -- add relativ adress to PC
              else
                     s_adr_mux <= "1011";        -- adress of CY      (CV: changed V1.5 according to specification: clear carry)
                     s_bdata_mux <= "0000";      -- bdata = 0    
                     s_regs_wr_en <= "110";      -- write one bit     
                s_pc_inc_en <= "0001";   -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_CJNE_ATRI_DATA =>       -- CJNE @Ri, #data, rel
            if state=FETCH then
              s_adr_mux <= "0111";        -- address = Ri-register
              s_pc_inc_en <= "0001";      -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= COMP_ROM_RAM;  -- Compare ROM_/RAM_DATA operation
              s_help_en <= "0010";        -- save aludata_i
              s_helpb_en <= '1';          -- save new_cy_i
              s_pc_inc_en <= "0001";      -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              if s_help /= 0 then
                s_adr_mux <= "1011";     -- adress of CY
                s_bdata_mux <= "0100";   -- bdata = s_helpb
                s_regs_wr_en <= "110";   -- write one bit
                s_pc_inc_en <= "0010";   -- add relativ adress to PC
              else
                     s_adr_mux <= "1011";        -- adress of CY      (CV: changed V1.5 according to specification: clear carry)
                     s_bdata_mux <= "0000";      -- bdata = 0    
                     s_regs_wr_en <= "110";      -- write one bit     
                s_pc_inc_en <= "0001";   -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_CLR_A =>              -- CLR A
            s_data_mux <= "0000";       -- data = 0
            s_regs_wr_en <= "010";      -- write ACC
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_CLR_C =>              -- CLR C
            s_adr_mux <= "1011";        -- adress of CY
            s_bdata_mux <= "0000";      -- bdata = 0
            s_regs_wr_en <= "110";      -- write one bit
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_CLR_BIT =>            -- CLR bit
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_bdata_mux <= "0000";    -- bdata = 0
              s_regs_wr_en <= "110";    -- write one bit
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_CPL_A =>              -- CPL A
            alu_cmd_o <= INV_ACC;       -- complement operation
            s_data_mux <= "0011";       -- data = aludata_i
            s_regs_wr_en <= "010";      -- write ACC
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_CPL_C =>              -- CPL C
            s_adr_mux <= "1011";        -- adress of CY
            s_bdata_mux <= "0101";      -- bdata = not cy
            s_regs_wr_en <= "110";      -- write one bit
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_CPL_BIT =>            -- CPL bit
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- bit adress
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1000";      -- bit adress
              s_bdata_mux <= "0110";    -- bdata = not s_bit_data
              s_regs_wr_en <= "110";    -- write one bit
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
           else
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_DA_A =>             -- DA A
            alu_cmd_o <= DA;          -- DA operation
            s_data_mux <= "0011";     -- data = aludata_i
            s_regs_wr_en <= "111";    -- write ACC and CY (special operation)
            s_pc_inc_en <= "0001";    -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_DEC_A =>              -- DEC A
            alu_cmd_o <= DEC_ACC;       -- decrement operation
            s_data_mux <= "0011";       -- data = aludata_i
            s_regs_wr_en <= "010";      -- write ACC
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_DEC_RR =>             -- DEC Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= DEC_RAM;     -- decrement operation
              s_adr_mux <= "0110";      -- address = rr-address
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_DEC_D =>              -- DEC direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              alu_cmd_o <= DEC_RAM;     -- decrement operation
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_DEC_ATRI =>           -- DEC @Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= DEC_RAM;     -- decrement operation
              s_adr_mux <= "0111";      -- address = ri-address
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_DIV_AB =>             -- DIV AB
            if state=FETCH then
              s_adr_mux <= "1100";      -- adress of B register
              alu_cmd_o <= DIV_ACC_RAM; -- divison operation
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0011";     -- data = aludata_i
              s_adr_mux <= "1100";      -- adress of B register
              alu_cmd_o <= DIV_ACC_RAM; -- divison operation
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_data_mux <= "0011";     -- data = aludata_i
              s_adr_mux <= "1100";      -- adress of B register
              s_regs_wr_en <= "111";    -- write ACC,B,OV,CY(special operation)
              alu_cmd_o <= DIV_ACC_RAM; -- divison operation
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_DJNZ_RR =>            -- DJNZ Rr, rel
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_help_en <= "0100";      -- save Rr-adress
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= DEC_RAM;     -- decrement operation
              if unsigned(aludata_i) /= 0 then
                s_adr_mux <= "1010";    -- address = rr-address
                s_data_mux <= "0011";   -- data = aludata_i
                s_regs_wr_en <= "100";  -- write one byte
                s_pc_inc_en <= "0010";  -- add relativ adress to PC
              else
                s_adr_mux <= "1010";    -- address = rr-address
                s_data_mux <= "0011";   -- data = aludata_i
                s_regs_wr_en <= "100";  -- write one byte
                s_pc_inc_en <= "0001";  -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_DJNZ_D =>             -- DJNZ direct, rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0001";      -- save address
              s_adr_mux <= "1000";      -- address = rom_data
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= DEC_RAM;            -- decrement operation
              if unsigned(aludata_i) /= 0 then
                s_adr_mux <= "1010";           -- address = help
                s_data_mux <= "0011";          -- data = aludata_i
                s_regs_wr_en <= "100";         -- write one byte
                s_pc_inc_en <= "0010";         -- add relativ adress to PC
              else
                s_adr_mux <= "1010";           -- address = help
                s_data_mux <= "0011";          -- data = aludata_i
                s_regs_wr_en <= "100";         -- write one byte
                s_pc_inc_en <= "0001";         -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_INC_A =>              -- INC A
            alu_cmd_o <= INC_ACC;       -- increment operation
            s_data_mux <= "0011";       -- data = aludata_i
            s_regs_wr_en <= "010";      -- write ACC
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_INC_RR =>             -- INC Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= INC_RAM;     -- increment operation
              s_adr_mux <= "0110";      -- address = rr-address
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_INC_D =>              -- INC direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= INC_RAM;     -- increment operation
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_INC_ATRI =>           -- INC @Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= INC_RAM;     -- increment operation
              s_adr_mux <= "0111";      -- address = Ri-register
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_INC_DPTR =>           -- INC DPTR
            if state=FETCH then
              s_adr_mux <= "1101";      -- adress of DPL
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= INC_RAM;     -- increment operation
              s_help_en <= "0010";      -- help = aludata_i
              s_adr_mux <= "1101";      -- adress of DPL
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1110";      -- adress of DPH
              s_nextstate <= EXEC3;
            elsif state=EXEC3 then
              if s_help=conv_unsigned(0,8) then
                alu_cmd_o <= INC_RAM;    -- increment operation
                s_adr_mux <= "1110";     -- adress of DPH
                s_data_mux <= "0011";    -- data = aludata_i
                s_regs_wr_en <= "100";   -- write one byte
              else
              end if;
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_JB =>                 -- JB bit, rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";       -- bit adress = rom_data_i
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              if s_bit_data = '1' then
                s_pc_inc_en <= "0010";  -- add relativ adress to PC
              else
                s_pc_inc_en <= "0001";  -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_JBC =>                -- JBC bit, rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- bit adress = rom_data_i
              s_help_en <= "0001";      -- s_help = rom_data_i
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1010";       -- s_adr = s_help
              s_bdata_mux <= "0000";     -- s_bdata = 0
              s_regs_wr_en <= "110";     -- write one bit
              if s_bit_data = '1' then
                s_pc_inc_en <= "0010";   -- add relativ adress to PC
              else
                s_pc_inc_en <= "0001";   -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_JC =>                 -- JC rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              if cy = '1' then
                s_pc_inc_en <= "0010";  -- add relativ adress to PC
              else
                s_pc_inc_en <= "0001";  -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_JMP_A_DPTR =>         -- JMP @A+DPTR
            s_pc_inc_en <= "0101";      -- PC = ACC + DPTR
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_JNB =>                -- JNB bit, rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";       -- bit adress = rom_data_i
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              if s_bit_data = '0' then
                s_pc_inc_en <= "0010";   -- add relativ adress to PC
              else
                s_pc_inc_en <= "0001";   -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_JNC =>                -- JNC rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              if cy = '0' then
                s_pc_inc_en <= "0010";  -- add relativ adress to PC
              else
                s_pc_inc_en <= "0001";  -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_JNZ =>                -- JNZ rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              if unsigned(acc) /= conv_unsigned(0,8) then
                s_pc_inc_en <= "0010";  -- add relativ adress to PC
              else
                s_pc_inc_en <= "0001";  -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_JZ =>                 -- JZ rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              if unsigned(acc) = conv_unsigned(0,8) then
                s_pc_inc_en <= "0010";         -- add relativ adress to PC
              else
                s_pc_inc_en <= "0001";         -- increment program-counter
              end if;
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_LCALL =>              -- LCALL addr16
            if state=FETCH then
              s_regs_wr_en <= "001";    -- increment stackpointer
              s_help16_en <= "01";      -- s_help16 <= pc + 3
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0001";      -- help <= rom_data_i
              s_data_mux <= "1100";     -- data <= help16(7 downto 0)
              s_adr_mux <= "0101";      -- s_adr <= sp
              s_regs_wr_en <= "101";    -- write one byte and increment SP
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_data_mux <= "1101";     -- data = help16(15 downto 8)
              s_adr_mux <= "0101";      -- s_adr <= sp
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0111";    -- load program counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_LJMP =>               -- LJMP addr16
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0001";      -- help = rom_data_i
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_pc_inc_en <= "0111";    -- load program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_A_RR =>           -- MOV A,Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_MOV_A_D =>            -- MOV A, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_A_ATRI =>         -- MOV A,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_A_DATA =>         -- MOV A, #DATA
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0101";     -- data = rom_data_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_RR_A =>           -- MOV Rr,A
            s_adr_mux <= "0110";        -- address = rr-address
            s_data_mux <= "0110";       -- data = ACC
            s_regs_wr_en <= "100";      -- write one byte
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_MOV_RR_D =>           -- MOV Rr, direct
            if state=FETCH then
              s_help_en <= "0100";      -- save Rr-adress in help
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1010";      -- address = help
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- do nothing with program counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_RR_DATA =>        -- MOV Rr,#data
            if state=FETCH then
              s_help_en <= "0100";      -- save Rr-adress in help
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1010";      -- address = help
              s_data_mux <= "0101";     -- data = rom_data_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- do nothing with program counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_D_A =>            -- MOV direct, A
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_data_mux <= "0110";     -- data = ACC
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

            -------------------------------------------------------------------

          when IC_MOV_D_RR =>           -- MOV direct, Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- do nothing with program counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_D_D =>            -- MOV direct, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_D_ATRI =>         -- MOV direct ,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_D_DATA =>         -- MOV direct, #data
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0001";      -- save direct adress
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1010";      -- address = help
              s_data_mux <= "0101";     -- data = rom_data_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_ATRI_A =>        -- MOV @Ri,A
            s_adr_mux <= "0111";       -- address = Ri-register
            s_data_mux <= "0110";      -- data = ACC
            s_regs_wr_en <= "100";     -- write one byte
            s_pc_inc_en <= "0001";     -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_MOV_ATRI_D =>         -- MOV @Ri, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_ATRI_DATA =>      -- MOV @Ri,#data
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_data_mux <= "0101";     -- data = rom_data_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOVC_A_ATDPTR =>       -- MOVC A, @A+DPTR
            if state=FETCH then
              s_help16_en <= "11";       -- save PC+1 in help16
              s_pc_inc_en <= "0101";     -- PC = A+DPTR
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0101";      -- data = rom_data_i
              s_regs_wr_en <= "010";     -- write ACC
              s_pc_inc_en <= "0110";     -- PC = help16
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOVC_A_ATPC =>         -- MOVC A, @A+PC
            if state=FETCH then
              s_help16_en <= "11";       -- save PC+1 in help16
              s_pc_inc_en <= "1001";     -- PC = A+PC+1
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0101";      -- data = rom_data_i
              s_regs_wr_en <= "010";     -- write ACC
              s_pc_inc_en <= "0110";     -- PC = help16
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOVX_A_ATRI =>         -- MOVX A, @RI
            if state=FETCH then
              s_adrx_mux <= "10";        -- external adress = Ri
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "1111";      -- data = datax_i
              s_regs_wr_en <= "010";     -- write ACC
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOVX_A_ATDPTR =>       -- MOVX A, @DPTR
            if state=FETCH then
              s_adrx_mux <= "01";        -- external adress = DPTR
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "1111";      -- data = datax_i
              s_regs_wr_en <= "010";     -- write ACC
              s_pc_inc_en <= "0001";     -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOVX_ATRI_A =>       -- MOVX @RI, A
            s_adrx_mux <= "10";        -- external adress = Ri
            s_wrx_mux <= '1';
            s_pc_inc_en <= "0001";     -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_MOVX_ATDPTR_A =>     -- MOVX @DPTR, A
            s_adrx_mux <= "01";        -- external adress = DPTR
            s_wrx_mux <= '1';
            s_pc_inc_en <= "0001";     -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_MOV_C_BIT =>          -- MOV C, bit
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_bdata_mux <= "0111";    -- bdata = s_bit_data
              s_regs_wr_en <= "110";    -- write one bit (automatic CY-address)
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_BIT_C =>          -- MOV bit,C
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_bdata_mux <= "1000";    -- bdata = cy
              s_regs_wr_en <= "110";    -- write one bit
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MOV_DPTR_DATA =>      -- MOV DPTR, #data16
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1110";      -- adress of DPH
              s_data_mux <= "0101";     -- data = rom_data_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1101";      -- adress of DPL
              s_data_mux <= "0101";     -- data = rom_data_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_MUL_AB =>             -- MUL AB
            if state=FETCH then
              s_adr_mux <= "1100";      -- adress of B register
              alu_cmd_o <= MUL_ACC_RAM; -- divison operation
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0011";     -- data = aludata_i
              s_adr_mux <= "1100";      -- adress of B register
              alu_cmd_o <= MUL_ACC_RAM; -- divison operation
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_data_mux <= "0011";    -- data = aludata_i
              s_adr_mux <= "1100";     -- adress of B register
              s_regs_wr_en <= "111";   -- write ACC,B,OV,CY (special operation)
              alu_cmd_o <= MUL_ACC_RAM; -- divison operation
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_NOP =>              -- NOP
            s_pc_inc_en <= "0001";    -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_ORL_A_RR =>           -- ORL A,Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= OR_RAM_ACC;  -- OR command (ACC v RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_ORL_A_D =>            -- ORL A, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= OR_RAM_ACC;  -- OR command (ACC v RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_ORL_A_ATRI =>         -- ORL A,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= OR_RAM_ACC;  -- OR command (ACC v RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_ORL_A_DATA =>         -- ORL A, data
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= OR_ROM_ACC;  -- OR command (ACC v ROM_DATA_I)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_ORL_D_A =>            -- ORL direct, A
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= OR_RAM_ACC;  -- OR command (ACC v RAM_DATA)
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ORL_D_DATA =>         -- ORL direct, DATA
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0001";      -- save rom_data_i
              s_adr_mux <= "1000";
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= OR_ROM_RAM;  -- OR command (ROM_DATA_I v RAM_DATA)
              s_adr_mux <= "1010";      -- address = help
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ORL_C_BIT =>          -- ORL C, bit
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_bdata_mux <= "1001";    -- bdata = s_bit_data or cy
              s_regs_wr_en <= "110";    -- write one bit (autmatically CY)
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_ORL_C_NBIT =>         -- ORL C, /bit
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_bdata_mux <= "1010";    -- bdata = not (s_bit_data or cy)
              s_regs_wr_en <= "110";    -- write one bit (autmatically CY)
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_POP =>                -- POP direct
            if state=FETCH then
              s_adr_mux <= "0101";      -- address = SP
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "101";    -- write one byte and decrement SP
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_PUSH =>               -- PUSH direct
            if state=FETCH then
              s_regs_wr_en <= "001";    -- increment SP
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "0101";      -- address = SP
              s_data_mux <= "0100";     -- data = s_reg_data
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_RET =>                -- RET
            if state=FETCH then
              s_adr_mux <= "0101";      -- adress = SP
              s_regs_wr_en <= "001";    -- decrement stackpointer
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0011";      -- save data for PC-high
              s_adr_mux <= "0101";      -- adress = SP
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_regs_wr_en <= "001";    -- decrement stackpointer
              s_pc_inc_en <= "1000";    -- reload program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_RETI =>               -- RETI
            if state=FETCH then
              s_adr_mux <= "0101";      -- adress = SP
              s_regs_wr_en <= "001";    -- decrement stackpointer
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0011";      -- save data for PC-high
              s_adr_mux <= "0101";
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              if s_inthigh='1' then     -- an high priority interrupt is ending
                s_inthigh_en <= '1';
                s_inthigh_d <= '0';
                s_ext0isrh_d   <= '0';
                s_ext0isrh_en  <= '1';
                s_ext1isrh_d   <= '0';
                s_ext1isrh_en  <= '1';
              elsif s_intlow='1' then   -- an low priority interrupt is ending
                s_intlow_en <= '1';
                s_intlow_d <= '0';
                s_ext0isr_d   <= '0';
                s_ext0isr_en  <= '1';
                s_ext1isr_d   <= '0';
                s_ext1isr_en  <= '1';                
              else                      -- no interrupt was started
                NULL;                   -- => normal RET command
              end if;
              s_regs_wr_en <= "001";    -- decrement stackpointer
              s_pc_inc_en <= "1000";    -- reload program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_RL_A =>               -- RL A
            alu_cmd_o <= RL_ACC;        -- rotate ACC left
            s_data_mux <= "0011";       -- data = aludata_i
            s_regs_wr_en <= "010";      -- write ACC
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_RLC_A =>              -- RLC A
            alu_cmd_o <= RLC_ACC;       -- rotate ACC with CY left
            s_data_mux <= "0011";       -- data = aludata_i
            s_regs_wr_en <= "111";      -- write ACC and CY (special operation)
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_RR_A =>               -- RR A
            alu_cmd_o <= RR_ACC;        -- rotate ACC right
            s_data_mux <= "0011";       -- data = aludata_i
            s_regs_wr_en <= "010";      -- write ACC
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_RRC_A =>              -- RRC A
            alu_cmd_o <= RRC_ACC;       -- rotate ACC with CY right
            s_data_mux <= "0011";       -- data = aludata_i
            s_regs_wr_en <= "111";      -- write ACC and CY (special operation)
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_SETB_C =>             -- SETB C
            s_adr_mux <= "1011";        -- adress of CY
            s_bdata_mux <= "1011";      -- bdata = 1
            s_regs_wr_en <= "110";      -- write one bit
            s_pc_inc_en <= "0001";      -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_SETB_BIT =>           -- SETB bit
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- byte adress
              s_bdata_mux <= "1011";    -- bdata = 1
              s_regs_wr_en <= "110";    -- write one bit
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_SJMP =>               -- SJMP rel
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_pc_inc_en <= "0010";    -- load program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_SUBB_A_RR =>          -- SUBB A,Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= SUB_ACC_RAM; -- subtraction command (ACC - RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_SUBB_A_D =>           -- SUBB A, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= SUB_ACC_RAM; -- subtraction command (ACC - RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_SUBB_A_ATRI =>        -- SUBB A,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= SUB_ACC_RAM; -- subtraction command (ACC - RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_SUBB_A_DATA =>        -- SUBB A, DATA
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "011";    -- write ACC and CY,OV,AC
              alu_cmd_o <= SUB_ACC_ROM; -- subtraction command (ACC-ROM_DATA_I)
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_SWAP_A =>         -- SWAP A
            s_data_mux <= "0111";   -- data = acc(3 downto 0) acc(7 downto 4)
            s_regs_wr_en <= "010";  -- write ACC
            s_pc_inc_en <= "0001";  -- increment program-counter
            s_nextstate <= FETCH;

          ---------------------------------------------------------------------

          when IC_XCH_A_RR =>           -- XCH A,Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "0110";      -- address = rr-address
              s_help_en <= "0011";      -- help = s_reg_data
              s_data_mux <= "0110";     -- data = ACC
              s_regs_wr_en <= "100";    -- write ACC
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_data_mux <= "1000";     -- data = help
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_XCH_A_D =>            -- XCH A, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1000";      -- address = rom_data_i
              s_help_en <= "0011";      -- help = s_reg_data
              s_data_mux <= "0110";     -- data = ACC
              s_regs_wr_en <= "100";    -- write one byte
              s_nextstate <= EXEC3;
            elsif state=EXEC3 then
              s_data_mux <= "1000";     -- data = help;
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_XCH_A_ATRI =>         -- XCH A,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_help_en <= "0011";      -- help = s_reg_data
              s_data_mux <= "0110";     -- data = ACC
              s_regs_wr_en <= "100";    -- write one byte
              s_nextstate <= EXEC2;             
            elsif state=EXEC2 then
              s_data_mux <= "1000";     -- data = help;
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_XCHD_A_ATRI =>        -- XCHD A,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_help_en <= "1010";      -- s_help = acc
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_data_mux <= "1010";     -- data = acc(7..4) & s_reg_data(3..0)
              s_regs_wr_en <= "010";    -- write ACC
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "0111";    -- address = Ri-register
              s_data_mux <= "1001";   -- data = s_reg_data(7..4) & s_help(3..0)
              s_regs_wr_en <= "100";  -- write one byte
              s_pc_inc_en <= "0001";  -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_XRL_A_RR =>           -- XRL A,Rr
            if state=FETCH then
              s_adr_mux <= "0110";      -- address = rr-address
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= XOR_RAM_ACC; -- XOR command (ACC v RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_XRL_A_D =>            -- XRL A, direct
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              alu_cmd_o <= XOR_RAM_ACC;   -- XOR command (ACC v RAM_DATA)
              s_data_mux <= "0011";       -- data = aludata_i
              s_regs_wr_en <= "010";      -- write ACC
              s_pc_inc_en <= "0001";      -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_XRL_A_ATRI =>         -- XRL A,@Ri
            if state=FETCH then
              s_adr_mux <= "0111";      -- address = Ri-register
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              alu_cmd_o <= XOR_RAM_ACC; -- XOR command (ACC v RAM_DATA)
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_XRL_A_DATA =>         -- XRL A, data
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "010";    -- write ACC
              alu_cmd_o <= XOR_ROM_ACC; -- XOR command (ACC v ROM_DATA_I)
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------
          when IC_XRL_D_A =>            -- XRL direct, A
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_adr_mux <= "1000";
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1000";
              s_data_mux <= "0011";     -- data = aludata_i
              s_regs_wr_en <= "100";    -- write one byte
              alu_cmd_o <= XOR_RAM_ACC; -- XOR command (ACC v RAM_DATA)
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when IC_XRL_D_DATA =>         -- XRL direct, DATA
            if state=FETCH then
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC1;
            elsif state=EXEC1 then
              s_help_en <= "0001";
              s_adr_mux <= "1000";
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= EXEC2;
            elsif state=EXEC2 then
              s_adr_mux <= "1010";
              s_data_mux <= "0011";     -- data = aludata_i
              alu_cmd_o <= XOR_ROM_RAM; -- XOR command (ROM_DATA_I v RAM_DATA)
              s_regs_wr_en <= "100";    -- write one byte
              s_pc_inc_en <= "0001";    -- increment program-counter
              s_nextstate <= FETCH;
            end if;

          ---------------------------------------------------------------------

          when others =>
            s_nextstate <= FETCH;
        end case;
      end if;
     end if;
  end process p_state;

end rtl;
