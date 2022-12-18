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
--         Filename:               mc8051_control_struc.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.7 $
--
--         Date of Latest Version: $Date: 2006-09-07 10:03:45 $
--
--
--         Description: Connects the units control_fsm and control_mem. This
--                      unit manages the whole microcontroller core.
--
--
--
--
-------------------------------------------------------------------------------
architecture struc of mc8051_control is

  -- signals connecting the fsm and the mem unit
  
  signal s_pc_inc_en  : std_logic_vector (3 downto 0);
  signal s_regs_wr_en : std_logic_vector (2 downto 0);
  signal s_data_mux   : std_logic_vector (3 downto 0);
  signal s_bdata_mux  : std_logic_vector (3 downto 0);
  signal s_adr_mux    : std_logic_vector (3 downto 0);
  signal s_adrx_mux   : std_logic_vector (1 downto 0);
  signal s_wrx_mux    : std_logic;
  signal s_help_en    : std_logic_vector (3 downto 0);
  signal s_help16_en  : std_logic_vector (1 downto 0);
  signal s_helpb_en   : std_logic;
  signal s_intpre2_d  : std_logic;
  signal s_intpre2_en : std_logic;
  signal s_intlow_d   : std_logic;
  signal s_intlow_en  : std_logic;
  signal s_inthigh_d  : std_logic;
  signal s_inthigh_en : std_logic;
  signal s_nextstate  : t_state;  	-- enable signal for state
  signal state        : t_state;  	-- actual state 
  signal s_command    : std_logic_vector (7 downto 0);
  signal s_help       : std_logic_vector(7 downto 0);  -- general help-register
  signal s_bit_data   : std_logic;
  signal s_intpre     : std_logic;  	-- an interrupt must start 
  signal s_intpre2    : std_logic;  	-- prepare for interrupt 
  signal s_inthigh    : std_logic;  	-- high priority int is running 
  signal s_intlow     : std_logic;  	-- low priority int is running 
  signal s_intblock   : std_logic;      -- interrupt delay at RETI, IE, IP
  signal s_ri         : std_logic;
  signal s_ti         : std_logic;
  signal s_tf1        : std_logic;
  signal s_tf0        : std_logic;
  signal s_ie1        : std_logic;
  signal s_ie0        : std_logic;
  signal ie           : std_logic_vector(7 downto 0);
  signal ip           : std_logic_vector(7 downto 0);
  signal psw          : std_logic_vector(7 downto 0);
  signal acc          : std_logic_vector(7 downto 0);
  signal s_ext0isr_d   : std_logic;
  signal s_ext1isr_d   : std_logic;
  signal s_ext0isrh_d  : std_logic;
  signal s_ext1isrh_d  : std_logic;
  signal s_ext0isr_en  : std_logic;
  signal s_ext1isr_en  : std_logic;
  signal s_ext0isrh_en : std_logic;
  signal s_ext1isrh_en : std_logic;


    
begin                 -- architecture structural

  acc_o <= acc;

  i_control_fsm : control_fsm
    port map(state_i    => state,
             help_i     => s_help,
             bit_data_i => s_bit_data,
             aludata_i  => aludata_i,
             command_i  => s_command,
             inthigh_i  => s_inthigh,
             intlow_i   => s_intlow,
             intpre_i   => s_intpre,
             intpre2_i  => s_intpre2,
             intblock_i => s_intblock,
             ti_i       => s_ti,
             ri_i       => s_ri,
             ie0_i      => s_ie0,
             ie1_i      => s_ie1,
             tf0_i      => s_tf0,
             tf1_i      => s_tf1,
             acc        => acc,
             psw        => psw,
             ie         => ie,
             ip         => ip,

             alu_cmd_o    => alu_cmd_o,
             pc_inc_en_o  => s_pc_inc_en,
             nextstate_o  => s_nextstate,
             adr_mux_o    => s_adr_mux,
             adrx_mux_o   => s_adrx_mux,
             wrx_mux_o    => s_wrx_mux,
             data_mux_o   => s_data_mux,
             bdata_mux_o  => s_bdata_mux,
             regs_wr_en_o => s_regs_wr_en,
             help_en_o    => s_help_en,
             help16_en_o  => s_help16_en,
             helpb_en_o   => s_helpb_en,
             inthigh_en_o => s_inthigh_en,
             intlow_en_o  => s_intlow_en,
             intpre2_en_o => s_intpre2_en,
             inthigh_d_o  => s_inthigh_d,
             intlow_d_o   => s_intlow_d,
             intpre2_d_o  => s_intpre2_d,
             ext0isr_d_o   => s_ext0isr_d  ,
             ext1isr_d_o   => s_ext1isr_d  ,
             ext0isrh_d_o  => s_ext0isrh_d ,
             ext1isrh_d_o  => s_ext1isrh_d ,
             ext0isr_en_o  => s_ext0isr_en ,
             ext1isr_en_o  => s_ext1isr_en ,
             ext0isrh_en_o => s_ext0isrh_en,
             ext1isrh_en_o => s_ext1isrh_en);
                   
                   
  i_control_mem : control_mem
    port map(pc_o           => pc_o,
             rom_data_i     => rom_data_i,
             ram_data_o     => ram_data_o,
             ram_data_i     => ram_data_i,
             ram_adr_o      => ram_adr_o,
             reg_data_o     => reg_data_o,
             ram_wr_o       => ram_wr_o,
             cy_o           => cy_o,
             ov_o           => ov_o,
             ram_en_o       => ram_en_o,
             aludata_i      => aludata_i,
             aludatb_i      => aludatb_i,
             acc_o          => acc,
             new_cy_i       => new_cy_i,
             new_ov_i       => new_ov_i,
             reset          => reset,
             clk            => clk,
             cen            => cen,
             int0_i         => int0_i,
             int1_i         => int1_i,
             p0_i           => p0_i,
             p1_i           => p1_i,
             p2_i           => p2_i,
             p3_i           => p3_i,
             p0_o           => p0_o,
             p1_o           => p1_o,
             p2_o           => p2_o,
             p3_o           => p3_o,
             all_trans_o    => all_trans_o,
             all_scon_o     => all_scon_o,
             all_sbuf_o     => all_sbuf_o,
             all_smod_o     => all_smod_o,
             all_scon_i     => all_scon_i,
             all_sbuf_i     => all_sbuf_i,
             all_tcon_tr0_o => all_tcon_tr0_o,
             all_tcon_tr1_o => all_tcon_tr1_o,
             all_tmod_o     => all_tmod_o,
             all_reload_o   => all_reload_o,
             all_wt_o       => all_wt_o,
             all_wt_en_o    => all_wt_en_o,
             all_tf0_i      => all_tf0_i,
             all_tf1_i      => all_tf1_i,
             all_tl0_i      => all_tl0_i,
             all_tl1_i      => all_tl1_i,
             all_th0_i      => all_th0_i,
             all_th1_i      => all_th1_i,

             state_o    => state,
             help_o     => s_help,
             bit_data_o => s_bit_data,
             command_o  => s_command,
             inthigh_o  => s_inthigh,
             intlow_o   => s_intlow,
             intpre_o   => s_intpre,
             intpre2_o  => s_intpre2,
             intblock_o => s_intblock,
             ti_o       => s_ti,
             ri_o       => s_ri,
             ie0_o      => s_ie0,
             ie1_o      => s_ie1,
             tf0_o      => s_tf0,
             tf1_o      => s_tf1,
             psw_o      => psw,
             ie_o       => ie,
             ip_o       => ip,
             adrx_o     => adrx_o,
             datax_o    => datax_o,
             wrx_o      => wrx_o,
             memx_o     => memx_o,

             datax_i      => datax_i,
             pc_inc_en_i  => s_pc_inc_en,
             nextstate_i  => s_nextstate,
             adr_mux_i    => s_adr_mux,
             adrx_mux_i   => s_adrx_mux,
             wrx_mux_i    => s_wrx_mux,
             data_mux_i   => s_data_mux,
             bdata_mux_i  => s_bdata_mux,
             regs_wr_en_i => s_regs_wr_en,
             help_en_i    => s_help_en,
             help16_en_i  => s_help16_en,
             helpb_en_i   => s_helpb_en,
             inthigh_en_i => s_inthigh_en,
             intlow_en_i  => s_intlow_en,
             intpre2_en_i => s_intpre2_en,
             inthigh_d_i  => s_inthigh_d,
             intlow_d_i   => s_intlow_d,
             intpre2_d_i  => s_intpre2_d,
             ext0isr_d_i   => s_ext0isr_d  ,
             ext1isr_d_i   => s_ext1isr_d  ,
             ext0isrh_d_i  => s_ext0isrh_d ,
             ext1isrh_d_i  => s_ext1isrh_d ,
             ext0isr_en_i  => s_ext0isr_en ,
             ext1isr_en_i  => s_ext1isr_en ,
             ext0isrh_en_i => s_ext0isrh_en,
             ext1isrh_en_i => s_ext1isrh_en);
      
end struc;
