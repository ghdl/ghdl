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
--         Filename:               mc8051_core_struc.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.9 $
--
--         Date of Latest Version: $Date: 2002-01-07 12:17:45 $
--
--
--         Description: Connect the mc8051_control and mc8051_alu modules.
--                      Generate and connect a certain number of mc8051_tmrctr
--                      and mc8051_siu units.
--
--
--
--
-------------------------------------------------------------------------------
architecture struc of mc8051_core is

  -- signals connecting the control unit with the rest
      
  signal s_reg_data:     std_logic_vector(7 downto 0);    -- data for ALU
  signal s_cy :          std_logic_vector(1 downto 0);    -- Carry Flag
  signal s_ov :          std_logic;                       -- Overflow Flag
  signal s_alu_cmd:      std_logic_vector (5 downto 0);   -- ALU operationscode
  signal s_alu_data0:    std_logic_vector (7 downto 0);   -- ALU result
  signal s_alu_data1:    std_logic_vector (7 downto 0);   -- 2nd ALU result
  signal s_acc:          std_logic_vector (7 downto 0);   -- ACC register 
  signal s_cyb:          std_logic_vector (1 downto 0);   -- CY result of ALU 
  signal s_ovb:          std_logic;                       -- OV result of ALU 
  signal s_reset:        std_logic;                       -- reset signal
  signal s_clk:          std_logic;                       -- clock signal
  
  -- signals to and from the SIUs
  
  signal s_all_trans    : std_logic_vector(C_IMPL_N_SIU-1 downto 0);
  signal s_all_scon     : std_logic_vector(6*C_IMPL_N_SIU-1 downto 0);
  signal s_all_sbuf     : std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);
  signal s_all_smod     : std_logic_vector(C_IMPL_N_SIU-1 downto 0);
  signal s_all_scon_out : std_logic_vector(3*C_IMPL_N_SIU-1 downto 0);
  signal s_all_sbuf_out : std_logic_vector(8*C_IMPL_N_SIU-1 downto 0);
        
  -- signals to and from the timer/counters
  
  signal s_all_tcon_tr0 : std_logic_vector(C_IMPL_N_TMR-1 downto 0);
  signal s_all_tcon_tr1 : std_logic_vector(C_IMPL_N_TMR-1 downto 0);
  signal s_all_tmod     : std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
  signal s_all_reload   : std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
  signal s_all_wt       : std_logic_vector(2*C_IMPL_N_TMR-1 downto 0);
  signal s_all_wt_en    : std_logic_vector(C_IMPL_N_TMR-1 downto 0);
  signal s_all_tf0      : std_logic_vector(C_IMPL_N_TMR-1 downto 0);
  signal s_all_tf1      : std_logic_vector(C_IMPL_N_TMR-1 downto 0);
  signal s_all_tl0      : std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
  signal s_all_th0      : std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
  signal s_all_tl1      : std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
  signal s_all_th1      : std_logic_vector(8*C_IMPL_N_TMR-1 downto 0);
   
begin                 -- architecture structural

  i_mc8051_control : mc8051_control
    port map(pc_o           => rom_adr_o,
             rom_data_i     => rom_data_i,
             ram_data_o     => ram_data_o,
             ram_data_i     => ram_data_i,
             ram_adr_o      => ram_adr_o,
             reg_data_o     => s_reg_data,
             ram_wr_o       => ram_wr_o,
             cy_o           => s_cy,
             ov_o           => s_ov,
             ram_en_o       => ram_en_o,
             alu_cmd_o      => s_alu_cmd,
             aludata_i      => s_alu_data0,
             aludatb_i      => s_alu_data1,
             acc_o          => s_acc,
             new_cy_i       => s_cyb,
             new_ov_i       => s_ovb,
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
             all_trans_o    => s_all_trans,
             all_scon_o     => s_all_scon,
             all_sbuf_o     => s_all_sbuf,
             all_smod_o     => s_all_smod,
             all_scon_i     => s_all_scon_out,
             all_sbuf_i     => s_all_sbuf_out,
             all_tcon_tr0_o => s_all_tcon_tr0,
             all_tcon_tr1_o => s_all_tcon_tr1,
             all_tmod_o     => s_all_tmod,
             all_reload_o   => s_all_reload,
             all_wt_o       => s_all_wt,
             all_wt_en_o    => s_all_wt_en,
             all_tf0_i      => s_all_tf0,
             all_tf1_i      => s_all_tf1,
             all_tl0_i      => s_all_tl0,
             all_tl1_i      => s_all_tl1,
             all_th0_i      => s_all_th0,
             all_th1_i      => s_all_th1,
             adrx_o         => adrx_o,
             datax_o        => datax_o,
             wrx_o          => wrx_o,
             memx_o         => memx_o,
             datax_i        => datax_i);
  
    
  i_mc8051_alu : mc8051_alu
    generic map (DWIDTH   => 8)
    port map(rom_data_i => rom_data_i,    -- inputs to mc8051_alu
             ram_data_i => s_reg_data,
             acc_i      => s_acc,
             cmd_i      => s_alu_cmd,
             cy_i       => s_cy,
             ov_i       => s_ov,
             
             result_a_o => s_alu_data0,   -- outputs of mc8051_alu
             result_b_o => s_alu_data1,
             new_cy_o   => s_cyb,
             new_ov_o   => s_ovb);


  gen_mc8051_siu : for i in c_impl_n_siu-1 downto 0 generate
    i_mc8051_siu : mc8051_siu
      port map (clk       => clk,        -- SIUs inputs
                cen       => cen,
                reset     => reset,
                tf_i      => s_all_tf1(i),
                trans_i   => s_all_trans(i),
                rxd_i     => all_rxd_i(i),
                scon_i    => s_all_scon((6*i)+5 downto i*6),
                sbuf_i    => s_all_sbuf((8*i)+7 downto i*8),
                smod_i    => s_all_smod(i),
                                         -- SIUs outputs
                sbuf_o    => s_all_sbuf_out((8*i)+7 downto i*8),
                scon_o    => s_all_scon_out((3*i)+2 downto i*3),
                rxdwr_o   => all_rxdwr_o(i),
                rxd_o     => all_rxd_o(i),
                txd_o     => all_txd_o(i));
  end generate;


  gen_mc8051_tmrctr : for i in c_impl_n_tmr-1 downto 0 generate
    i_mc8051_tmrctr : mc8051_tmrctr
      port map (clk        => clk,       -- tmr_ctr inputs
                cen        => cen,
                reset      => reset,
                int0_i     => int0_i(i),
                int1_i     => int1_i(i), 
                t0_i       => all_t0_i(i),
                t1_i       => all_t1_i(i), 
                tmod_i     => s_all_tmod((8*i)+7 downto i*8),
                tcon_tr0_i => s_all_tcon_tr0(i),
                tcon_tr1_i => s_all_tcon_tr1(i),
                reload_i   => s_all_reload((8*i)+7 downto i*8),
                wt_en_i    => s_all_wt_en(i),
                wt_i       => s_all_wt((2*i)+1 downto i*2),
                                         -- tmr_ctr outputs
                th0_o      => s_all_th0((8*i)+7 downto i*8),
                tl0_o      => s_all_tl0((8*i)+7 downto i*8),
                th1_o      => s_all_th1((8*i)+7 downto i*8),
                tl1_o      => s_all_tl1((8*i)+7 downto i*8),
                tf0_o      => s_all_tf0(i),
                tf1_o      => s_all_tf1(i));
  end generate;

  
end struc;
