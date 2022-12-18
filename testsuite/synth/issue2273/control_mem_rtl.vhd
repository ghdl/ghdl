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
--         Filename:               control_mem_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.11 $
--
--         Date of Latest Version: $Date: 2010-03-24 10:20:48 $
--
--
--         Description: Describe all sequential funcitonality like read from
--                      special function registers, observe interrupt sources,
--                      write to special function registers, and read or write
--                      to the bit addressable memory area.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of control_mem is  

 
   type    t_gprbit is array (15 downto 0) of unsigned(7 downto 0); 
   subtype muxint is integer range C_IMPL_N_TMR-1 downto 0;

   signal s_help:        unsigned (7 downto 0);   -- general help-register 
   signal s_help16:      unsigned (15 downto 0);  -- 16 bit help-register
   signal s_helpb :      std_logic;               -- general help-bit 
   signal s_ir:          unsigned (7 downto 0);   -- reg for saving the command
   signal gprbit:        t_gprbit;         -- bitadressable general purpose RAM
   signal s_r0_b0:	 unsigned (7 downto 0);   -- Register R0 / Bank 0
   signal s_r1_b0:	 unsigned (7 downto 0);   -- Register R1 / Bank 0
   signal s_r0_b1:	 unsigned (7 downto 0);   -- Register R0 / Bank 1
   signal s_r1_b1:	 unsigned (7 downto 0);   -- Register R1 / Bank 1
   signal s_r0_b2:	 unsigned (7 downto 0);   -- Register R0 / Bank 2
   signal s_r1_b2:	 unsigned (7 downto 0);   -- Register R1 / Bank 2
   signal s_r0_b3:	 unsigned (7 downto 0);   -- Register R0 / Bank 3
   signal s_r1_b3:	 unsigned (7 downto 0);   -- Register R1 / Bank 3
   signal s_reg_data:    unsigned (7 downto 0);   -- equals reg_data_o 
   Signal state:         t_state;                 -- actual state 
   signal s_command:     std_logic_vector (7 downto 0); 

   signal s_pc_inc_en  : std_logic_vector (3 downto 0); 
   signal s_regs_wr_en : std_logic_vector (2 downto 0); 
   signal s_data_mux   : std_logic_vector (3 downto 0);
   signal s_bdata_mux  : std_logic_vector (3 downto 0);
   signal s_adr_mux    : std_logic_vector (3 downto 0);
   signal s_adrx_mux   : std_logic_vector (1 downto 0);
   signal s_help_en    : std_logic_vector (3 downto 0);
   signal s_help16_en  : std_logic_vector (1 downto 0);
   signal s_helpb_en   : std_logic;
   signal s_intpre2_d : std_logic;
   signal s_intpre2_en: std_logic;
   signal s_intlow_d  : std_logic;
   signal s_intlow_en : std_logic;
   signal s_inthigh_d : std_logic;
   signal s_inthigh_en: std_logic;
   signal s_ext0isr_d  : std_logic;
   signal s_ext0isrh_d : std_logic;
   signal s_ext1isr_d  : std_logic;
   signal s_ext1isrh_d : std_logic;

   signal s_nextstate : t_state;              -- enable signal for state

   signal s_bit_data :   std_logic; 
   signal s_intpre:      std_logic;      -- an interrupt must start 
   signal s_intpre2:     std_logic;      -- prepare for interrupt 
   signal s_inthigh:     std_logic;      -- high priority int is running 
   signal s_intlow:      std_logic;      -- low priority int is running 
   signal s_intblock:    std_logic;      -- interrupt delay at RETI, IE, IP
   signal s_intblock_o:  std_logic;      -- CK, CV: intblock_o signal for internal use

   signal s_int0_edge :  t_ext_l; 
   signal s_int1_edge :  t_ext_l; 

   signal s_tf0_edge :   t_tmr_l; 
   signal s_tf1_edge :   t_tmr_l; 
   signal s_ri_edge  :   t_siu_l; 
   signal s_ti_edge  :   t_siu_l;
   signal s_smodreg  :   t_siu_l;
   signal s_tl0      :   t_tmr_us;
   signal s_tl1      :   t_tmr_us;
   signal s_th0      :   t_tmr_us;
   signal s_th1      :   t_tmr_us;
   signal s_sbufi    :   t_siu_us;
   signal s_reload   :   t_tmr_us;
   signal s_wt       :   t_tmr_us2;
   
   signal s_tf1 :        std_logic;
   signal s_tf0 :        std_logic;
   signal s_ie1 :        std_logic;
   signal s_ie0 :        std_logic;
   
   signal s_ri : std_logic;
   signal s_ti : std_logic;
   signal s_rb8 : std_logic;
   signal s_tb8 : std_logic;
   signal s_ren : std_logic;
   signal s_sm2 : std_logic;
   signal s_sm1 : std_logic;
   signal s_sm0 : std_logic;
   signal s_smod : std_logic;
 
   signal s_int0_h1 : t_ext_l;          -- help-bit for edge detection
   signal s_int0_h2 : t_ext_l; 
   signal s_int0_h3 : t_ext_l; 
   signal s_int1_h1 : t_ext_l;
   signal s_int1_h2 : t_ext_l; 
   signal s_int1_h3 : t_ext_l; 

   signal s_tf0_h1,s_tf0_h2 :         t_tmr_l; 
   signal s_tf1_h1,s_tf1_h2 :         t_tmr_l; 
   signal s_ri_h1,s_ri_h2 :           t_siu_l; 
   signal s_ti_h1,s_ti_h2 :           t_siu_l; 
 
   signal s_tsel : muxint;
   signal s_ssel : muxint;
 
   signal s_p :          std_logic; 
   
   signal s_p0 :         std_logic_vector(7 downto 0);
   signal s_p1 :         std_logic_vector(7 downto 0);
   signal s_p2 :         std_logic_vector(7 downto 0);
   signal s_p3 :         std_logic_vector(7 downto 0);
      
   signal pc:            unsigned(15 downto 0);   -- program counter register
   signal pc_comb:       unsigned(15 downto 0);   -- program counter
   signal pc_plus1:      unsigned(15 downto 0);   -- program counter + 1
   signal pc_plus2:      unsigned(15 downto 0);   -- program counter + 2

   signal s_data    : unsigned(7 downto 0); 
   signal s_adr     : unsigned(7 downto 0); 
   signal s_preadr  : unsigned(7 downto 0); 
   signal s_bdata   : std_logic; 
   signal s_rr_adr  : unsigned (7 downto 0);
   signal s_ri_adr  : std_logic_vector (7 downto 0);
   signal s_ri_data : unsigned (7 downto 0);
  
 -- 8051 standard special-function-register (SFR)
 
   signal p0:           unsigned(7 downto 0); 
   signal sp:           unsigned(7 downto 0); 
   signal dpl:          unsigned(7 downto 0); 
   signal dph:          unsigned(7 downto 0); 
   signal pcon:         unsigned(3 downto 0); 
   signal tcon:         t_tmr_lv;
   signal tmod:         t_tmr_us;
   signal p1:           unsigned(7 downto 0); 
   signal scon:         t_siu_lv; 
   signal sbuf:         t_siu_us; 
   signal p2:           unsigned(7 downto 0); 
   signal ie:           std_logic_vector(7 downto 0); 
   signal p3:           unsigned(7 downto 0); 
   signal ip:           std_logic_vector(7 downto 0); 
   signal psw:          std_logic_vector(7 downto 0); 
   signal acc:          unsigned(7 downto 0); 
   signal b:            unsigned(7 downto 0); 
 
 -- 8051 extended special-function-register
 
   signal tsel:         unsigned(7 downto 0);          -- select a Timer-Unit 
   signal ssel:         unsigned(7 downto 0);          -- select a SIU-Unit 

   alias CY : std_logic is psw(7); 
   alias AC : std_logic is psw(6); 
   alias OV : std_logic is psw(2); 
   alias EA:  std_logic is ie(7); 
   alias ES:  std_logic is ie(4); 
   alias ET1: std_logic is ie(3); 
   alias EX1: std_logic is ie(2); 
   alias ET0: std_logic is ie(1); 
   alias EX0: std_logic is ie(0); 
   alias PS0: std_logic is ip(4); 
   alias PT1: std_logic is ip(3); 
   alias PX1: std_logic is ip(2); 
   alias PT0: std_logic is ip(1); 
   alias PX0: std_logic is ip(0); 

begin 

  -- some simple assignments 
 
  pc_o <= std_logic_vector(pc_comb);
  pc_plus1 <= pc + conv_unsigned(1,1);
  pc_plus2 <= pc + conv_unsigned(2,2);
  ram_adr_o <= std_logic_vector(s_adr(6 downto 0)); 
  reg_data_o <= std_logic_vector(s_reg_data); 
  ram_data_o <= std_logic_vector(s_data);
  acc_o <= std_logic_vector(acc); 
  cy_o(1) <= cy; 
  ov_o <= ov; 
  cy_o(0) <= ac; 
  
  ie_o <= ie;
  ip_o <= ip;
  psw_o <= psw;
  state_o <= state;
  command_o <= s_command;
  ri_o <= s_ri;
  ti_o <= s_ti;
  help_o <= std_logic_vector(s_help);
  bit_data_o <= s_bit_data;
  intpre_o <= s_intpre;
  intpre2_o <= s_intpre2;
  inthigh_o <= s_inthigh;
  intlow_o <= s_intlow;
  tf1_o <= s_tf1;
  tf0_o <= s_tf0;
  ie1_o <= s_ie1;
  ie0_o <= s_ie0;

  s_pc_inc_en <= pc_inc_en_i;   
  s_nextstate <= nextstate_i;    
  s_adr_mux <=	adr_mux_i;     
  s_adrx_mux <=	adrx_mux_i;     
  s_data_mux <=	data_mux_i;    
  s_bdata_mux <= bdata_mux_i;   
  s_regs_wr_en <= regs_wr_en_i;  
  s_help_en <=	help_en_i;     
  s_help16_en <= help16_en_i;   
  s_helpb_en <=	helpb_en_i;    
  s_inthigh_en <= inthigh_en_i;  
  s_intlow_en <=  intlow_en_i;   
  s_intpre2_en <= intpre2_en_i;  
  s_inthigh_d <=  inthigh_d_i;   
  s_intlow_d <=   intlow_d_i;    
  s_intpre2_d <=  intpre2_d_i;   
  intblock_o <= s_intblock_o;                  -- CK, CV: assignment of intblock_o output

  s_tsel <= conv_integer(tsel) when tsel < C_IMPL_N_TMR
         else 0;      -- selected timer unit is (not) implemented
  s_ssel <= conv_integer(ssel) when ssel < C_IMPL_N_SIU
         else 0;      -- selected siu unit is (not) implemented
  
  for_tmr:
  for i in 0 to C_IMPL_N_TMR-1 generate
    all_tcon_tr0_o(i) <= tcon(i)(4); 
    all_tcon_tr1_o(i) <= tcon(i)(6);   
    all_tmod_o((i*8)+7 downto i*8) <= std_logic_vector(tmod(i)); 
    s_tl0(i) <= unsigned(all_tl0_i((i*8)+7 downto i*8));
    s_tl1(i) <= unsigned(all_tl1_i((i*8)+7 downto i*8));
    s_th0(i) <= unsigned(all_th0_i((i*8)+7 downto i*8));
    s_th1(i) <= unsigned(all_th1_i((i*8)+7 downto i*8));
    all_reload_o((i*8)+7 downto i*8) <= std_logic_vector(s_reload(i));
    all_wt_o((i*2)+1 downto i*2) <= std_logic_vector(s_wt(i));
  end generate for_tmr;

  s_tf1 <= tcon(s_tsel)(7);
  s_tf0 <= tcon(s_tsel)(5);
  s_ie1 <= tcon(s_tsel)(3);
  s_ie0 <= tcon(s_tsel)(1);
  
  for_siu:
  for i in 0 to C_IMPL_N_SIU-1 generate
    all_scon_o((6*i)+5) <= scon(i)(0);          -- RI
    all_scon_o((6*i)+4) <= scon(i)(7);          -- SM0
    all_scon_o((6*i)+3) <= scon(i)(6);          -- SM1
    all_scon_o((6*i)+2) <= scon(i)(5);          -- SM2
    all_scon_o((6*i)+1) <= scon(i)(4);          -- REN
    all_scon_o(6*i) <= scon(i)(3);              -- TB8
    all_smod_o(i) <= s_smodreg(i);              -- SMOD
    all_sbuf_o((8*i)+7 downto 8*i) <= std_logic_vector(sbuf(i)); 
    s_sbufi(i) <= unsigned(all_sbuf_i((i*8)+7 downto i*8));
  end generate for_siu;

  s_sm0 <= scon(s_ssel)(7);
  s_sm1 <= scon(s_ssel)(6);
  s_sm2 <= scon(s_ssel)(5);
  s_ren <= scon(s_ssel)(4);
  s_tb8 <= scon(s_ssel)(3);
  s_rb8 <= all_scon_i((s_ssel*3)+2);
  s_ti  <= scon(s_ssel)(1);
  s_ri  <= scon(s_ssel)(0);
  s_smod<= s_smodreg(s_ssel);
     
  p0_o <= std_logic_vector(p0); 
  p1_o <= std_logic_vector(p1); 
  p2_o <= std_logic_vector(p2); 
  p3_o <= std_logic_vector(p3);
 
  s_p <= acc(7) xor acc(6) xor acc(5) xor acc(4) xor 
         acc(3) xor acc(2) xor acc(1) xor acc(0); 
                                    -- P should be set, if the count 
                                    -- of 1 in the acc is even   
 
  s_command <= rom_data_i when state=FETCH 
         else conv_std_logic_vector(s_ir,8); 

  s_rr_adr <= unsigned((psw and "00011000") or (rom_data_i  
              and "00000111"));     -- calculate registerdirect-adress
             
  s_ri_adr <= ((psw and "00011000") or (s_command(7 downto 0) and "00000001"));
        
  datax_o <= std_logic_vector(acc);
  wrx_o <= wrx_mux_i;  

  -- This logic ensures that a pending interrupt is not serviced if
  -- a) RETI instruction is in progress,
  -- b) any write to IE or IP register is in progress.

  --s_intblock <= --'1' when (std_logic_vector(s_ir) = RETI) or -- RETI in progress       -- CK,CV: new logic for s_intblock inserted
               
                --(s_regs_wr_en = "100" and             -- \
                -- (conv_integer(s_adr) = 16#B8# or     -- | write to 
                --  conv_integer(s_adr) = 16#A8#) and   -- | IE or IP
                -- s_intpre2 = '0') or                  -- / 
                --(s_regs_wr_en = "110" and                       -- \
                --  (s_adr(7 downto 3) = conv_unsigned(21,5) or   -- | bit access
                --  s_adr(7 downto 3) = conv_unsigned(23,5)) and -- | to IE v IP
                -- s_intpre2 = '0') else '0';                     -- / 

--        s_intblock <= '1' when (std_logic_vector(s_command) = RETI)         
--        else '0' when (s_nextstate = FETCH and (std_logic_vector(s_command) /= RETI)) 
--        else s_intblock_o;
               
    process(s_command, s_regs_wr_en, s_adr, s_intpre2, s_intblock_o, s_nextstate)
    
    begin

        if (std_logic_vector(s_command) = RETI)       -- ensure single instruction execution after RETI command 
          or
            (s_regs_wr_en = "100" and                   -- register access to IP or I
            (conv_integer(s_adr) = 16#B8# or conv_integer(s_adr) = 16#A8#) and 
             s_intpre2 = '0') 
          or
            (s_regs_wr_en = "110" and                   -- bitr access to IE or IP
            (s_adr(7 downto 3) = conv_unsigned(21,5) or s_adr(7 downto 3) = conv_unsigned(23,5)) and
             s_intpre2 = '0')
          then  s_intblock <= '1';
          
        elsif (s_nextstate = FETCH and (std_logic_vector(s_command) /= RETI)) 
          then  s_intblock <= '0';
             
        else s_intblock <= s_intblock_o;        

       end if;
    end process;                                                                        -- CK,CV: end of logic insertion


------------------------------------------------------------------------------ 
-- purpose: process to read SFR, bitadressable or normal RAM
-- inputs:  s_adr,s_preadr,sp,dpl,dph,pcon,tcon,tmod,all_tl0_i,
--          all_tl1_i,all_th0_i,all_th1_i,s_p0,s_p1,all_scon_i,all_sbuf_i,
--          s_p2,ie,s_p3,ip,psw,acc,b,gprbit,ram_data_i
-- outputs: s_reg_data, s_bit_data
------------------------------------------------------------------------------ 
 
  p_readram : process (s_preadr,s_p0,sp,dpl,dph,pcon,tcon,tmod,s_p1,scon,s_p2,
                       ie,s_p3,ip,psw,acc,b,gprbit,ram_data_i,s_r0_b0,s_r1_b0,
                       s_r0_b1,s_r1_b1,s_r0_b2,s_r1_b2,s_r0_b3,s_r1_b3,
                       s_smod,s_sm0,s_sm1,s_sm2,s_ren,s_tb8,s_rb8,s_ti,s_ri,
                       s_sbufi,s_th1,s_th0,s_ssel,s_tsel,s_tl1,s_tl0,
		       ssel,tsel) 
  begin  
    if s_preadr(7)='1' then 
      case conv_integer(s_preadr) is         -- read one byte of a SFR  
            when 16#80# => s_reg_data <= unsigned(s_p0);  
            when 16#81# => s_reg_data <= sp; 
            when 16#82# => s_reg_data <= dpl; 
            when 16#83# => s_reg_data <= dph; 
            when 16#87# => 
              s_reg_data(7) <= s_smod;
              s_reg_data(6 downto 4) <= "000";
              s_reg_data(3 downto 0) <= pcon;
            when 16#88# => s_reg_data <= unsigned(tcon(s_tsel));
            when 16#89# => s_reg_data <= unsigned(tmod(s_tsel));
            when 16#8A# => s_reg_data <= s_tl0(s_tsel);
            when 16#8B# => s_reg_data <= s_tl1(s_tsel);
            when 16#8C# => s_reg_data <= s_th0(s_tsel);
            when 16#8D# => s_reg_data <= s_th1(s_tsel);
            when 16#8E# => s_reg_data <= tsel;             
            when 16#90# => s_reg_data <= unsigned(s_p1); 
            when 16#98# => 
              s_reg_data(0) <= s_ri;    -- from SCON register 
              s_reg_data(1) <= s_ti;    -- from SCON register                
              s_reg_data(2) <= s_rb8;   -- read extern input!!! 
              s_reg_data(3) <= s_tb8;   
              s_reg_data(4) <= s_ren;   
              s_reg_data(5) <= s_sm2;   
              s_reg_data(6) <= s_sm1;   
              s_reg_data(7) <= s_sm0;   
            when 16#99# => s_reg_data <= s_sbufi(s_ssel);
            when 16#9A# => s_reg_data <= ssel; 
            when 16#A0# => s_reg_data <= unsigned(s_p2); 
            when 16#A8# => s_reg_data <= unsigned(ie);
            when 16#B0# => s_reg_data <= unsigned(s_p3); 
            when 16#B8# => s_reg_data <= unsigned(ip);
            when 16#D0# => s_reg_data <= unsigned(psw);
            when 16#E0# => s_reg_data <= acc; 
            when 16#F0# => s_reg_data <= b;     
            when others => s_reg_data <= conv_unsigned(0,8); 
          end case; 
                                 -- read one byte to bitadressable GPR 
    elsif conv_std_logic_vector(s_preadr(7 downto 4),4)="0010" then   
            s_reg_data <= gprbit(conv_integer(s_preadr(3 downto 0)));  
             
    elsif conv_std_logic_vector(s_preadr,8)="00000000" then
            s_reg_data <= s_r0_b0;     -- read R0 / Bank 0
    elsif conv_std_logic_vector(s_preadr,8)="00000001" then
            s_reg_data <= s_r1_b0;     -- read R1 / Bank 0
    elsif conv_std_logic_vector(s_preadr,8)="00001000" then
            s_reg_data <= s_r0_b1;     -- read R0 / Bank 1
    elsif conv_std_logic_vector(s_preadr,8)="00001001" then
            s_reg_data <= s_r1_b1;     -- read R1 / Bank 1
    elsif conv_std_logic_vector(s_preadr,8)="00010000" then
            s_reg_data <= s_r0_b2;     -- read R0 / Bank 2
    elsif conv_std_logic_vector(s_preadr,8)="00010001" then
            s_reg_data <= s_r1_b2;     -- read R1 / Bank 2
    elsif conv_std_logic_vector(s_preadr,8)="00011000" then
            s_reg_data <= s_r0_b3;     -- read R0 / Bank 3
    elsif conv_std_logic_vector(s_preadr,8)="00011001" then
            s_reg_data <= s_r1_b3;     -- read R1 / Bank 3
     
    else                               -- read on general purpose RAM 
            s_reg_data <= unsigned(ram_data_i); 
    end if; 

    if s_preadr(7)='1' then 
      case s_preadr(6 downto 3) is    -- read only one bit from a SFR 
        when "0000" => s_bit_data <= s_p0(conv_integer(s_preadr(2 downto 0))); 
        when "0001" => 
          s_bit_data <= tcon(s_tsel)(conv_integer(s_preadr(2 downto 0))); 
        when "0010" => s_bit_data <= s_p1(conv_integer(s_preadr(2 downto 0))); 
        when "0011" => 
          if conv_integer(s_preadr(2 downto 0))=2 then 
            s_bit_data <= s_rb8; 
          else 
            s_bit_data <= scon(s_ssel)(conv_integer(s_preadr(2 downto 0))); 
          end if; 
        when "0100" => s_bit_data <= s_p2(conv_integer(s_preadr(2 downto 0))); 
        when "0101" => s_bit_data <= ie(conv_integer(s_preadr(2 downto 0))); 
        when "0110" => s_bit_data <= s_p3(conv_integer(s_preadr(2 downto 0))); 
        when "0111" => s_bit_data <= ip(conv_integer(s_preadr(2 downto 0))); 
        when "1010" => s_bit_data <= psw(conv_integer(s_preadr(2 downto 0))); 
        when "1100" => s_bit_data <= acc(conv_integer(s_preadr(2 downto 0))); 
        when "1110" => s_bit_data <= b(conv_integer(s_preadr(2 downto 0))); 
        when others => s_bit_data <= '0'; 
      end case; 
    else                               -- read one bit from bitadressable GP 
            s_bit_data <= gprbit(conv_integer(s_preadr(6 downto 3))) 
                          (conv_integer(s_preadr(2 downto 0)));  
    end if;           
  end process p_readram; 

------------------------------------------------------------------------------ 
-- detect the rising/falling edge of interupt-sources
------------------------------------------------------------------------------ 

for_intext_edge:
  for i in 0 to C_IMPL_N_EXT-1 generate
    -- falling edge of int0_i
    s_int0_edge(i) <= not s_int0_h2(i) and s_int0_h3(i);
    -- falling edge of int1_i
    s_int1_edge(i) <= not s_int1_h2(i) and s_int1_h3(i);
  end generate for_intext_edge;
    
for_tf_edge:       
  for i in 0 to C_IMPL_N_TMR-1 generate
    -- on rising edge of tf0_i
    s_tf0_edge(i) <= s_tf0_h1(i) and not s_tf0_h2(i);
    -- on rising edge of tf1_i
    s_tf1_edge(i) <= s_tf1_h1(i) and not s_tf1_h2(i);
  end generate for_tf_edge;
    
for_siu_edge:       
  for i in 0 to C_IMPL_N_SIU-1 generate
    -- on rising edge of RI
    s_ri_edge(i) <= s_ri_h1(i) and not s_ri_h2(i);
    -- on rising edge of TI
    s_ti_edge(i) <= s_ti_h1(i) and not s_ti_h2(i);
  end generate for_siu_edge;   

 
------------------------------------------------------------------------------ 
-- purpose: write some help-regs for detecting the falling/rising edge
--          of interupt-sources
-- inputs:  clk,reset,int0_i,int1_i,all_tf0_i,all_tf1_i,all_scon_i,
--          s_tf0_h1,s_tf1_h1,s_ri_h1,s_ti_h1 
-- outputs: s_int0_h1,s_int0_h2,s_int0_h3,s_int1_h1,s_int1_h2,s_int1_h3,
--          s_tf0_h1,s_tf0_h2
--          s_tf1_h1,s_tf1_h2,s_ri_h1,s_ri_h2,s_ti_h1,s_ti_h2
------------------------------------------------------------------------------ 
    
  iep: process (clk,cen,reset) 
 
  begin  -- process iep 
    
    -- activities triggered by asynchronous reset 
    if reset = '1' then 
      s_int0_h1 <= (others => '1'); 
      s_int0_h2 <= (others => '1'); 
      s_int0_h3 <= (others => '1'); 
      s_int1_h1 <= (others => '1'); 
      s_int1_h2 <= (others => '1'); 
      s_int1_h3 <= (others => '1'); 
      s_tf0_h1 <= (others => '0'); 
      s_tf0_h2 <= (others => '0'); 
      s_tf1_h1 <= (others => '0'); 
      s_tf1_h2 <= (others => '0'); 
      s_ri_h1 <= (others => '0'); 
      s_ri_h2 <= (others => '0'); 
      s_ti_h1 <= (others => '0'); 
      s_ti_h2 <= (others => '0');
      s_intblock_o <= '0';                          -- CK, CV: changed s_inblock_o to s_intblock_o for internal use
    -- activities triggered by rising edge of clock 
    elsif clk'event and clk = '1' and cen = '1' then
      s_intblock_o <= s_intblock;                   -- CK, CV: changed s_inblock_o to s_intblock_o for internal use
      for i in 0 to C_IMPL_N_EXT-1 loop
        s_int0_h1(i) <= int0_i(i);              -- external INT0 
        s_int0_h2(i) <= s_int0_h1(i); 
        s_int0_h3(i) <= s_int0_h2(i); 
        s_int1_h1(i) <= int1_i(i);              -- external INT1 
        s_int1_h2(i) <= s_int1_h1(i);
        s_int1_h3(i) <= s_int1_h2(i);
      end loop; 
      for i in 0 to C_IMPL_N_TMR-1 loop
        s_tf0_h1(i) <= all_tf0_i(i);          -- TF0 flags 
        s_tf0_h2(i) <= s_tf0_h1(i); 
        s_tf1_h1(i) <= all_tf1_i(i);          -- TF1 flags 
        s_tf1_h2(i) <= s_tf1_h1(i);      
        -- all_xxx is a std_logic_vector, s_xxx is a array of std_logic !!!
      end loop;
      for i in 0 to C_IMPL_N_SIU-1 loop
        s_ri_h1(i) <= all_scon_i(i*3);        -- RI flag
        s_ri_h2(i) <= s_ri_h1(i); 
        s_ti_h1(i) <= all_scon_i((i*3)+1);    -- TI flag 
        s_ti_h2(i) <= s_ti_h1(i);      
        -- all_xxx is a std_logic_vector, s_xxx is a array of std_logic !!!
      end loop;
    end if; 
  end process iep; 
 
------------------------------------------------------------------------------ 
-- purpose: check the interupt-sources and start an interupt if necessary
-- inputs:  ie,ip,scon,tcon,s_inthigh,s_intlow,s_ti,s_ri,s_ie0,
--          s_ie1,s_tf0,s_tf1
-- outputs: s_intpre
------------------------------------------------------------------------------ 
 
  intpre : process (ie,ip,scon,tcon,s_inthigh,s_intlow,s_ti,s_ri,s_ie0,
                    s_ie1,s_tf0,s_tf1) 
  begin   
    if (EA='1' and (EX0='1' or ET0='1' or EX1='1' or ET1='1' or ES='1')) then 
      if s_inthigh='1' then       -- interrupt with high priority is running 
        s_intpre <= '0';           
      elsif s_intlow='1' then     -- interrupt with low priority is running 
        if ((PX0 and EX0 and s_ie0) or 
            (PT0 and ET0 and s_tf0) or 
            (PX1 and EX1 and s_ie1) or 
            (PT1 and ET1 and s_tf1) or 
            (PS0 and ES and (s_ri or s_ti)))='1' then 
          s_intpre <= '1';        -- a second interrupt must start 
        else                      
          s_intpre <= '0'; 
        end if; 
      else                        -- no interrupt is running 
        if ((EX0 and s_ie0) or 
            (ET0 and s_tf0) or
            (EX1 and s_ie1) or
            (ET1 and s_tf1) or
            (ES and (s_ri or s_ti)))='1' then 
          s_intpre <= '1';        -- an interrupt must start 
        else 
          s_intpre <= '0'; 
        end if; 
      end if; 
    else                          -- no interrupt must be carried out
      s_intpre <= '0'; 
    end if;     
  end process intpre; 

------------------------------------------------------------------------------ 
-- purpose: this are the main-multiplexer for reading and writing regs
-- inputs:  s_data_mux,pc,aludata_i,s_reg_data,rom_data_i,acc,
--          s_bdata_mux,s_bit_data,psw,new_cy_i,s_helpb,sp,s_rr_adr,
--          s_ri_data,s_help,s_adr_mux,s_adr,s_regs_wr_en,pc_plus2,
--          s_help16,s_ri_adr,s_r0_b0,s_r1_b0,s_r0_b1,s_r1_b1,
--          s_r0_b2,s_r1_b2,s_r0_b3,s_r1_b3
-- outputs: s_data,s_bdata,s_adr,s_ri_data,ram_wr_o
------------------------------------------------------------------------------ 
 
  p_multiplexer : process (s_data_mux,pc,aludata_i,s_reg_data,rom_data_i,acc,
                     s_bdata_mux,s_bit_data,psw,new_cy_i,s_helpb,sp,s_rr_adr,
                     s_ri_data,s_help,s_adr_mux,s_adr,s_regs_wr_en,pc_plus2,
                     s_help16,s_ri_adr,s_r0_b0,s_r1_b0,s_r0_b1,s_r1_b1,
                     s_r0_b2,s_r1_b2,s_r0_b3,s_r1_b3,s_adrx_mux,dph,dpl,
		     datax_i) 
  begin
    case s_data_mux is
      when "0000" => s_data <= conv_unsigned(0,8);
      when "0001" => s_data <= pc(7 downto 0);
      when "0010" => s_data <= pc(15 downto 8);
      when "0011" => s_data <= unsigned(aludata_i);
      when "0100" => s_data <= s_reg_data;
      when "0101" => s_data <= unsigned(rom_data_i);
      when "0110" => s_data <= acc;
      when "0111" => 
        s_data(7 downto 4) <= acc(3 downto 0);
        s_data(3 downto 0) <= acc(7 downto 4);
      when "1000" => s_data <= s_help;
      when "1001" => 
        s_data(7 downto 4) <= s_reg_data(7 downto 4);
        s_data(3 downto 0) <= s_help(3 downto 0);
      when "1010" => 
        s_data(7 downto 4) <= acc(7 downto 4);
        s_data(3 downto 0) <= s_reg_data(3 downto 0);
      when "1100" => s_data <= s_help16(7 downto 0);
      when "1101" => s_data <= s_help16(15 downto 8);
      when "1110" => s_data <= pc_plus2(7 downto 0);
      when "1111" => s_data <= unsigned(datax_i);
      when others => s_data <= conv_unsigned(0,8);
    end case;

    case s_bdata_mux is
      when "0000" => s_bdata <= '0';
      when "0001" => s_bdata <= s_bit_data and cy;
      when "0010" => s_bdata <= not(s_bit_data) and cy;
      when "0011" => s_bdata <= new_cy_i(1);
      when "0100" => s_bdata <= s_helpb;
      when "0101" => s_bdata <= not cy;
      when "0110" => s_bdata <= not s_bit_data;
      when "0111" => s_bdata <= s_bit_data;
      when "1000" => s_bdata <= cy;
      when "1001" => s_bdata <= s_bit_data or cy;
      when "1010" => s_bdata <= not(s_bit_data) or cy;
      when "1011" => s_bdata <= '1';
      when others => s_bdata <= '0';
    end case;

    case s_adr_mux is
      when "0000" => s_adr <= conv_unsigned(0,8);
      when "0001" => s_adr <= conv_unsigned(16#89#,8); 
      when "0010" => s_adr <= conv_unsigned(16#8D#,8); 
      when "0011" => s_adr <= conv_unsigned(16#8B#,8);
      when "0100" => s_adr <= conv_unsigned(16#8F#,8); 
      when "0101" => s_adr <= sp;
      when "0110" => s_adr <= s_rr_adr;
      when "0111" => s_adr <= s_ri_data;
      when "1000" => s_adr <= unsigned(rom_data_i);
      when "1001" => s_adr <= s_reg_data;
      when "1010" => s_adr <= s_help ;
      when "1011" => s_adr <= conv_unsigned(16#D7#,8); 
      when "1100" => s_adr <= conv_unsigned(16#F0#,8);
      when "1101" => s_adr <= conv_unsigned(16#82#,8); 
      when "1110" => s_adr <= conv_unsigned(16#83#,8); 
      when "1111" => s_adr <= sp + conv_unsigned(1,1); 
      when others => s_adr <= conv_unsigned(0,8);
    end case;

    case s_adrx_mux is
      --when "00" => adrx_o <= (others => '0');
      when "01" => 
        adrx_o(15 downto 8) <= std_logic_vector(dph); 
        adrx_o(7 downto 0) <= std_logic_vector(dpl); 
        memx_o <= '1';
      when "10" => 
        adrx_o(15 downto 8) <= (others => '0'); 
        adrx_o(7 downto 0) <= std_logic_vector(s_ri_data); 
        memx_o <= '1';
      when others => adrx_o <= (others => '0'); memx_o <= '0';
    end case;
          
    case s_ri_adr is
      when "00000000" => s_ri_data <= s_r0_b0;    
      when "00000001" => s_ri_data <= s_r1_b0;    
      when "00001000" => s_ri_data <= s_r0_b1;    
      when "00001001" => s_ri_data <= s_r1_b1;    
      when "00010000" => s_ri_data <= s_r0_b2;    
      when "00010001" => s_ri_data <= s_r1_b2;    
      when "00011000" => s_ri_data <= s_r0_b3;    
      when "00011001" => s_ri_data <= s_r1_b3;
      when others => s_ri_data <= conv_unsigned(0,8);
    end case;   

    if ((s_regs_wr_en="100") or (s_regs_wr_en="101")) then    -- write one byte
      if ((s_adr(7)='1') or                                   -- SFR
        (conv_std_logic_vector(s_adr(7 downto 4),4)="0010")   -- bitadressable
        or ((std_logic_vector(s_adr) AND "11100110") = "00000000"))   -- R0,R1
      then
         ram_wr_o <= '0';
      else
        ram_wr_o <= '1';                -- to RAM-block
      end if;
    else
      ram_wr_o <= '0';
    end if;
  end process p_multiplexer; 

------------------------------------------------------------------------------ 
-- purpose: writes internal registers, on which the user have no 
--          direct access
-- inputs:  reset,clk,s_nextstate,state,s_help_en,
--          rom_data_i,aludata_i,s_reg_data,s_rr_adr,new_cy_i
-- outputs: state,s_ir,s_help,s_help16,s_helpb,s_intpre2,s_intlow,
--          s_inthigh,s_preadr
------------------------------------------------------------------------------ 
 
  p_wr_internal_reg : process (reset,cen,clk)
  begin
    if reset='1' then 
      state <= STARTUP; 
      s_ir <= conv_unsigned(0,8); 
      s_help <= conv_unsigned(0,8);
      s_help16 <= conv_unsigned(0,16);
      s_helpb <= '0';
      s_intpre2 <= '0';
      s_intlow <= '0';
      s_inthigh <= '0';
      s_preadr <= conv_unsigned(0,8);
      pc <= conv_unsigned(0,16); 
      s_p0 <= "11111111";
      s_p1 <= "11111111";
      s_p2 <= "11111111";
      s_p3 <= "11111111";
      s_ext0isr_d <= '0';
      s_ext0isrh_d <= '0';
      s_ext1isr_d <= '0';
      s_ext1isrh_d <= '0';
    else
      if Rising_Edge(clk) and cen='1' then  

        state <= s_nextstate;                        -- update current state

        if state=FETCH then 
          s_ir <= unsigned(rom_data_i);              -- save OP-Code in IR 
        end if; 
        
        case s_help_en is
          when "0000" => NULL;
          when "0001" => s_help <= unsigned(rom_data_i);
          when "0010" => s_help <= unsigned(aludata_i);
          when "0011" => s_help <= s_reg_data;
          when "0100" => s_help <= s_rr_adr;
          when "0101" => s_help <= conv_unsigned(16#03#,8);
          when "0110" => s_help <= conv_unsigned(16#0B#,8);
          when "0111" => s_help <= conv_unsigned(16#13#,8); 
          when "1000" => s_help <= conv_unsigned(16#1B#,8);
          when "1001" => s_help <= conv_unsigned(16#23#,8);
          when "1010" => s_help <= acc;
          when others => NULL;
        end case;
    
        case s_help16_en is
          when "00" => NULL;
          when "01" => s_help16 <= pc + conv_unsigned(3,2);
          when "10" => s_help16 <= pc_plus2;
          when "11" => s_help16 <= pc_plus1;
          when others => NULL;
        end case;

        case s_helpb_en is
          when '0' => NULL;
          when '1' => s_helpb <= new_cy_i(1);
          when others => NULL;
        end case;

        if s_intpre2_en='1' then           -- help-reg for s_intpre
          s_intpre2 <= s_intpre2_d;
        else
          NULL;
        end if;

        if s_intlow_en='1' then
          s_intlow <= s_intlow_d;
        else
          NULL;
        end if;

        if s_inthigh_en='1' then
          s_inthigh <= s_inthigh_d;
        else
          NULL;
        end if;

        if ext0isr_en_i = '1' then
          s_ext0isr_d <= ext0isr_d_i;
        end if;
        if ext1isr_en_i = '1' then
          s_ext1isr_d <= ext1isr_d_i;
        end if;
        if ext0isrh_en_i = '1' then
          s_ext0isrh_d <= ext0isrh_d_i;
        end if;
        if ext1isrh_en_i = '1' then
          s_ext1isrh_d <= ext1isrh_d_i;
        end if;

        s_preadr <= s_adr;

        pc <= pc_comb;               -- write pc-register

	s_p0 <= p0_i;
	s_p1 <= p1_i;
	s_p2 <= p2_i;
	s_p3 <= p3_i;

      end if;
    end if;
  end process p_wr_internal_reg; 

------------------------------------------------------------------------------ 
-- purpose: calculate the next PC-adress 
-- inputs:  s_pc_inc_en,pc_plus1,rom_data_i,s_intpre2,s_help,s_command,
--          s_help16,s_ir,dph,dpl,acc,s_reg_data,pc
-- outputs: pc_comb
------------------------------------------------------------------------------ 
 
  p_pc : process (s_pc_inc_en,pc_plus1,rom_data_i,s_help,
                  s_help16,s_ir,dph,dpl,acc,s_reg_data,pc) 
  variable v_dptr: unsigned(15 downto 0);
  begin
    v_dptr(15 downto 8) := dph;
    v_dptr(7 downto 0) := dpl;
    case s_pc_inc_en is 
      when "0001" =>                  -- increment PC 
        pc_comb <= pc_plus1; 
      when "0010" =>                  -- for relativ jumps and calls 
        pc_comb <= conv_unsigned(pc_plus1 + signed(rom_data_i),16);  
      when "0011" =>                  -- load interrupt vectoradress
        pc_comb(15 downto 8) <= conv_unsigned(0,8);
        pc_comb(7 downto 0) <= s_help; 
      when "0100" =>                  -- ACALL and AJMP
        pc_comb(15 downto 11) <= s_help16(15 downto 11);
        pc_comb(10 downto 8) <= s_ir(7 downto 5);
        pc_comb(7 downto 0) <= unsigned(rom_data_i);
      when "0101" =>                  -- JMP_A_DPTR, MOVC_A_ATDPTR
        pc_comb <= v_dptr + conv_unsigned(acc,8); 
      when "0110" =>                  -- MOVC
        pc_comb <= s_help16;
      when "0111" =>                  -- LJMP, LCALL
        pc_comb(15 downto 8) <= s_help;
        pc_comb(7 downto 0) <= unsigned(rom_data_i);
      when "1000" =>                  -- RET, RETI
        pc_comb(15 downto 8) <= s_help;
        pc_comb(7 downto 0) <= s_reg_data;
      when "1001" =>                  -- MOVC_A_ATPC
        pc_comb <= pc_plus1 + conv_unsigned(acc,8);
      when others => pc_comb <= pc;
    end case;
  end process p_pc;
  
------------------------------------------------------------------------------ 
-- purpose: write RAM, SFR or bitadressable Registers
-- inputs:  reset,clk
-- outputs: SFR, gprbit
------------------------------------------------------------------------------ 
 
  p_write_ram : process (reset,clk,cen) 
  begin
    if reset='1' then 
      ram_en_o <= '0'; 
      gprbit(0) <= conv_unsigned(0,8);                                        
      gprbit(1) <= conv_unsigned(0,8);                                        
      gprbit(2) <= conv_unsigned(0,8);                                        
      gprbit(3) <= conv_unsigned(0,8);                                        
      gprbit(4) <= conv_unsigned(0,8);                                        
      gprbit(5) <= conv_unsigned(0,8);                                        
      gprbit(6) <= conv_unsigned(0,8);                                        
      gprbit(7) <= conv_unsigned(0,8);                                        
      gprbit(8) <= conv_unsigned(0,8);                                        
      gprbit(9) <= conv_unsigned(0,8);                                        
      gprbit(10) <= conv_unsigned(0,8);                                        
      gprbit(11) <= conv_unsigned(0,8);                                        
      gprbit(12) <= conv_unsigned(0,8);                                        
      gprbit(13) <= conv_unsigned(0,8);                                        
      gprbit(14) <= conv_unsigned(0,8);                                        
      gprbit(15) <= conv_unsigned(0,8);
      s_r0_b0 <= conv_unsigned(0,8);
      s_r1_b0 <= conv_unsigned(0,8);
      s_r0_b1 <= conv_unsigned(0,8);
      s_r1_b1 <= conv_unsigned(0,8);
      s_r0_b2 <= conv_unsigned(0,8);
      s_r1_b2 <= conv_unsigned(0,8);
      s_r0_b3 <= conv_unsigned(0,8);
      s_r1_b3 <= conv_unsigned(0,8);
      p0 <= conv_unsigned(255,8);                            
      sp <= conv_unsigned(7,8);    
      dpl <= conv_unsigned(0,8);                           
      dph <= conv_unsigned(0,8);                           
      pcon <= conv_unsigned(0,4);                          
      tcon <= (others => (others => '0'));                          
      tmod <= (others => (others => '0'));                          
      tsel <= conv_unsigned(0,8);                          
      p1 <= conv_unsigned(255,8);                            
      scon <= (others => (others => '0'));                           
      s_smodreg <= (others => '0');
      sbuf <= (others => (others => '0'));
      ssel <= conv_unsigned(0,8);                          
      p2 <= conv_unsigned(255,8);                            
      ie <= conv_std_logic_vector(0,8);
      p3 <= conv_unsigned(255,8);                            
      ip <= conv_std_logic_vector(0,8);                            
      psw <= conv_std_logic_vector(0,8);  
      acc <= conv_unsigned(0,8);                           
      b <= conv_unsigned(0,8); 
      s_reload <= (others => (others => '0'));
      all_wt_en_o <= (others => '0'); 
      s_wt <= (others => (others => '0')); 
      all_trans_o <= (others => '0');
    else 
      ram_en_o <= '1'; 

      if Rising_Edge(clk) and cen='1' then  
        all_wt_en_o <= ( others => '0' );              -- default values 
        all_trans_o <= ( others => '0' ); 
        psw(0) <= s_p; 
        for i in 0 to C_IMPL_N_TMR-1 loop
          tcon(i)(3) <= tcon(i)(3) or ((tcon(i)(2) and s_int1_edge(i)) or 
                        ((not tcon(i)(2) and not s_int1_h2(i)) and
                         not(s_ext1isr_d or s_ext1isrh_d)));
          tcon(i)(1) <= tcon(i)(1) or ((tcon(i)(0) and s_int0_edge(i)) or 
                        ((not tcon(i)(0) and not s_int0_h2(i)) and
                         not(s_ext0isr_d or s_ext0isrh_d)));
          tcon(i)(5) <= tcon(i)(5) or s_tf0_edge(i);
          tcon(i)(7) <= tcon(i)(7) or s_tf1_edge(i);
        end loop;
        -- update TF1, TF0 and IE1, IE0 in dependance on the IT flags 
        for i in 0 to C_IMPL_N_SIU-1 loop
          scon(i)(0) <= scon(i)(0) or s_ri_edge(i);
          scon(i)(1) <= scon(i)(1) or s_ti_edge(i);
        end loop;

        case s_regs_wr_en is 
          when "001" =>                 -- inc or dec SP

            if (s_intblock='0' and s_intpre='1' and state=FETCH) or s_intpre2='1' then
              sp <= sp + conv_unsigned(1,1);
            else
              case s_command is 
                when RET | RETI => 
                  sp <= sp - conv_unsigned(1,1); 
                when others => 
                  sp <= sp + conv_unsigned(1,1); 
              end case;
            end if;
          when "010" =>                 -- write to ACC
            acc <= s_data; 
          when "011" =>                 -- write to ACC and PSW
            acc <= s_data; 
            psw(7) <= new_cy_i(1);
            psw(6) <= new_cy_i(0);
            psw(2) <= new_ov_i;
          when "100" | "101" =>
            case s_regs_wr_en is                  -- sp register
              when "100" =>
                if conv_integer(s_adr) = 16#81# then
                  sp <= s_data;
                end if;
              when "101" =>
                if (s_intblock='0' and s_intpre='1' and state=FETCH) or s_intpre2='1' then
                  sp <= sp + conv_unsigned(1,1);
                else
                  if s_command = POP then
                    if (conv_integer(s_adr) = 16#81#) then
                      sp <= s_data;
                    else
                      sp <= sp - conv_unsigned(1, 1);
                    end if;
                  else
                    sp <= sp + conv_unsigned(1, 1);
                  end if;
                end if;
              when others => NULL;
            end case;
            
            if s_adr(7)='1' then 
              case conv_integer(s_adr) is         -- write one byte of a SFR  
              when 16#80# => p0 <= s_data;                
              when 16#82# => dpl <= s_data; 
              when 16#83# => dph <= s_data; 
              when 16#87# => 
                pcon <= s_data(3 downto 0); 
                s_smodreg(s_ssel) <= s_data(7);
              when 16#88# => tcon(s_tsel) <= std_logic_vector(s_data);
              when 16#89# => tmod(s_tsel) <= s_data;
              when 16#8A# => 
                  s_reload(s_tsel) <= s_data;  -- tl0 
                  s_wt(s_tsel) <= "00"; 
                  all_wt_en_o(s_tsel) <= '1'; 
              when 16#8B# => 
                  s_reload(s_tsel) <= s_data;  -- tl1
                  s_wt(s_tsel) <= "01"; 
                  all_wt_en_o(s_tsel) <= '1'; 
              when 16#8C# => 
                  s_reload(s_tsel) <= s_data;  -- th0 
                  s_wt(s_tsel) <= "10"; 
                  all_wt_en_o(s_tsel) <= '1'; 
              when 16#8D# => 
                  s_reload(s_tsel) <= s_data;  -- th1 
                  s_wt(s_tsel) <= "11"; 
                  all_wt_en_o(s_tsel) <= '1'; 
              when 16#8E# => tsel <= s_data; 
              when 16#90# => p1 <= s_data; 
              when 16#98# => scon(s_ssel) <= std_logic_vector(s_data);
              when 16#99# => 
                sbuf(s_ssel) <= s_data;
                all_trans_o(s_ssel) <= '1'; 
              when 16#9A# => ssel <= s_data; 
              when 16#A0# => p2 <= s_data; 
              when 16#A8# => ie <= conv_std_logic_vector(s_data,8); 
              when 16#B0# => p3 <= s_data; 
              when 16#B8# => ip <= conv_std_logic_vector(s_data,8); 
              when 16#D0# =>  
                psw(7 downto 1) <= conv_std_logic_vector(s_data(7 downto 1),7);
              when 16#E0# => acc <= s_data; 
              when 16#F0# => b <= s_data;     
              when others => NULL; 
              end case; 
                                   -- write one byte to bitadressable GPR 
            elsif conv_std_logic_vector(s_adr(7 downto 4),4)="0010" then   
              gprbit(conv_integer(s_adr(3 downto 0))) <= s_data; 
            elsif conv_std_logic_vector(s_adr,8)="00000000" then
              s_r0_b0 <= s_data;         -- write R0 / Bank 0
            elsif conv_std_logic_vector(s_adr,8)="00000001" then
              s_r1_b0 <= s_data;         -- write R1 / Bank 0
            elsif conv_std_logic_vector(s_adr,8)="00001000" then
              s_r0_b1 <= s_data;         -- write R0 / Bank 1
            elsif conv_std_logic_vector(s_adr,8)="00001001" then
              s_r1_b1 <= s_data;         -- write R1 / Bank 1
            elsif conv_std_logic_vector(s_adr,8)="00010000" then
              s_r0_b2 <= s_data;         -- write R0 / Bank 2
            elsif conv_std_logic_vector(s_adr,8)="00010001" then
              s_r1_b2 <= s_data;         -- write R1 / Bank 2
            elsif conv_std_logic_vector(s_adr,8)="00011000" then
              s_r0_b3 <= s_data;         -- write R0 / Bank 3
            elsif conv_std_logic_vector(s_adr,8)="00011001" then
              s_r1_b3 <= s_data;         -- write R1 / Bank 3
            else                               -- write on general purpose RAM 
              NULL;
            end if; 
          when "110" => 
            if (s_intblock='0' and s_intpre='1' and state=FETCH) or s_intpre2='1' or
               (s_command /= ANL_C_BIT and
                s_command /= ANL_C_NBIT and
                s_command /= MOV_C_BIT and
                s_command /= ORL_C_BIT and
                s_command /= ORL_C_NBIT) then
              if s_adr(7)='1' then 
                case s_adr(6 downto 3) is  -- write only one bit of an SFR 
                  when "0000" => 
                    p0(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when "0001" => 
                    tcon(s_tsel)(conv_integer(s_adr(2 downto 0))) <= s_bdata;
                  when "0010" => 
                    p1(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when "0011" => 
                    scon(s_ssel)(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when "0100" => 
                    p2(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when "0101" => 
                    ie(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when "0110" => 
                    p3(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when "0111" => 
                    ip(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when "1010" =>  
                    case s_adr(2 downto 0) is 
                      when "000" => NULL; 
                      when "001" => psw(1) <= s_bdata;
                      when "010" => psw(2) <= s_bdata;
                      when "011" => psw(3) <= s_bdata;
                      when "100" => psw(4) <= s_bdata;
                      when "101" => psw(5) <= s_bdata;
                      when "110" => psw(6) <= s_bdata;
                      when "111" => psw(7) <= s_bdata;
                      when others => NULL; 
                    end case; 
                  when "1100" => 
                    acc(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when "1110" => 
                    b(conv_integer(s_adr(2 downto 0))) <= s_bdata; 
                  when others => NULL; 
                end case; 
              else                  -- write one bit to bitadressable GP 
                gprbit(conv_integer(s_adr(6 downto 3))) 
                      (conv_integer(s_adr(2 downto 0))) <= s_bdata;  
              end if;
            else
              psw(7) <= s_bdata;               -- write CY
            end if;
          when "111" => 
            case s_command is 
              when DA_A | RLC_A | RRC_A =>     -- write ACC, CY 
                acc <= s_data; 
                psw(7) <= new_cy_i(1);
              when DIV_AB | MUL_AB =>          -- write ACC, B, CY, OV 
                acc <= s_data; 
                b <= unsigned(aludatb_i); 
                psw(7) <= '0';
                psw(2) <= new_ov_i;
              when others => NULL; 
            end case; 
          when others => NULL;
        end case; 
      end if;
    end if; 
  end process p_write_ram; 

end rtl;  
  
