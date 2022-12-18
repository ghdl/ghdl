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
--         Filename:               mc8051_siu_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.10 $
--
--         Date of Latest Version: $Date: 2010-03-24 10:20:48 $
--
--
--         Description: Serial interface unit for the mc8051 microcontroller.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of mc8051_siu is

  signal s_rxpre_count  : unsigned(5 downto 0);  -- Receive prescaler
  signal s_txpre_count  : unsigned(5 downto 0);  -- Transmit prescaler
  signal s_m0_shift_en  : std_logic;             -- masks out every twelfth
                                                 -- rising edge of clk
  signal s_m2_rxshift_en  : std_logic;           -- mode 2 shift enable
  signal s_m13_rxshift_en : std_logic;           -- mode 1 and 3 shift enable
  signal s_m2_txshift_en  : std_logic;           -- mode 2 shift enable
  signal s_m13_txshift_en : std_logic;           -- mode 1 and 3 shift enable
  signal s_ff0          : std_logic;             -- flipflop for edge dedection
  signal s_ff1          : std_logic;             -- flipflop for edge dedection
  signal s_tf           : std_logic;             -- synchronised timer flag
  signal s_mode         : unsigned(1 downto 0);  -- mode
  signal s_sm2          : std_logic;             -- multi processor comm. bit
  signal s_detect       : std_logic;             -- indicates start of recept. 
  signal s_ren          : std_logic;             -- receive enable
  signal s_rxd_val      : std_logic;             -- received data bit
  signal s_txdm0        : std_logic;             -- shift clock for m0
  signal s_ri           : std_logic;             -- external receive interrupt 
  signal s_trans        : std_logic;             -- enable transmission 
  signal s_recv_done    : std_logic;             -- receive interrupt
  signal s_tran_done    : std_logic;             -- transmit interrupt
  signal s_rb8          : std_logic;             -- 8th data bit
  signal s_tb8          : std_logic;             -- 8th data bit
  signal s_recv_state   : unsigned(3 downto 0);  -- state reg. of receive unit
  signal s_tran_state   : unsigned(3 downto 0);  -- state reg. of transmit unit
  signal s_rxd_ff0      : std_logic;             -- sample flip-flop
  signal s_rxd_ff1      : std_logic;             -- sample flip-flop
  signal s_rxd_ff2      : std_logic;             -- sample flip-flop
  signal s_det_ff0      : std_logic;             -- rec. detect flip-flop
  signal s_det_ff1      : std_logic;             -- rec. detect flip-flop
  signal s_tran_sh      : unsigned(10 downto 0); -- transmission shift register
  signal s_recv_sh      : unsigned(7 downto 0);  -- reception shift register
  signal s_recv_buf     : unsigned(7 downto 0);  -- reception buffer register
  signal s_rxm13_ff0    : std_logic;             -- generates an enable singal
  signal s_rxm13_ff1    : std_logic;             -- generates an enable singal
  signal s_txm13_ff0    : std_logic;             -- generates an enable singal
  signal s_txm13_ff1    : std_logic;             -- generates an enable singal
  
begin                 -- architecture rtl
    
  s_mode(1) <= scon_i(4);               -- defines the 4 operating modes
  s_mode(0) <= scon_i(3); 
  s_ren  <= scon_i(1);                  -- receive enable
  s_sm2  <= scon_i(2);                  -- 1 time or half time baud rate
  s_tb8  <= scon_i(0);                  -- 9th data bit for transmission
  s_ri   <= scon_i(5);                  -- the receive interrupt bit of the
                                        -- control unit
  sbuf_o <= std_logic_vector(s_recv_buf);  -- the receive buffer output
  scon_o(0) <= s_recv_done;             -- set when reception is completed
  scon_o(1) <= s_tran_done;             -- set when transmission is completed
  scon_o(2) <= s_rb8;                   -- 9th data bit of reception  

-------------------------------------------------------------------------------
  -- The two flip flops are updated every rising clock edge of clk.
  -- If a rising edge
  -- on the port tf_i is dedected the signal s_tf is set to 1 for one period.
  --
  -- The transmission start signal s_trans is generated and held high till
  -- the statemachine has been launched with its first shift.
  --
  -- The shift clock for mode0 is generated. It toggles with the half
  -- s_m0_shift_en rate.
  
  s_tf <= '1' when (s_ff0 = '1' and s_ff1 = '0') else '0';      

  p_sample_tf: process (clk, cen, reset)
      
    begin

      if reset = '1' then
        s_ff0 <= '0';
        s_ff1 <= '0';
        s_trans <= '0';
      else
        if clk'event and clk = '1' and cen='1' then
          s_ff0 <= tf_i;
          s_ff1 <= s_ff0;
          
          if trans_i = '1' then
            s_trans <= '1';
          else
            case s_mode is
              when ("00") =>
                if s_m0_shift_en = '1' then
                  s_trans <= '0';
                end if;
              when ("01") => 
                if s_m13_txshift_en = '1' then
                  s_trans <= '0';
                end if;
              when ("10") => 
                if s_m2_txshift_en = '1' then
                  s_trans <= '0';
                end if;
              when others => 
                if s_m13_txshift_en = '1' then
                  s_trans <= '0';
                end if;
            end case;
          end if;
        end if;    
      end if;    

  end process p_sample_tf;
      

-------------------------------------------------------------------------------
  -- The register s_rxpre_count is driven with the system clock clk. So a
  -- good enable signal (which is stable when clk has its rising edge) can be
  -- derived to mask out every pulse of clk needed.
  -- s_m0_shift_en activates every twelfth clock cycle
  -- s_m2_shift_en activates baud rates of 1/32 or 1/64 the clock frequenzy
  -- depending on signal smod_i
  -- s_m13_shift_en activates baud rates depending on timer/counter1 flag
      
  s_m0_shift_en <= '1' when s_txpre_count(3 downto 0) = conv_unsigned(11,5)
                   else '0';
      
  s_m2_rxshift_en <= '1' when (s_rxpre_count(4 downto 0) = conv_unsigned(31,5)
                               and smod_i = '1') or
                              (s_rxpre_count = conv_unsigned(63,6)
                               and smod_i = '0')
                   else '0';
  s_m13_rxshift_en <= '1' when s_rxm13_ff0 = '1' and s_rxm13_ff1 = '0'
                      else '0';

  s_m2_txshift_en <= '1' when (s_txpre_count(4 downto 0) = conv_unsigned(31,5)
                               and smod_i = '1') or
                              (s_txpre_count = conv_unsigned(63,6)
                               and smod_i = '0')
                   else '0';
  s_m13_txshift_en <= '1' when s_txm13_ff0 = '1' and s_txm13_ff1 = '0'
                      else '0';
      
  p_divide_clk: process (clk, cen, reset)
    
    begin

      if reset = '1' then
        s_rxpre_count <= conv_unsigned(0,6);
        s_txpre_count <= conv_unsigned(0,6);
        s_rxm13_ff0 <= '0';
        s_rxm13_ff1 <= '0';
        s_txm13_ff0 <= '0';
        s_txm13_ff1 <= '0';
      else
        if clk'event and clk='1' and cen='1' then
            
          s_rxm13_ff1 <= s_rxm13_ff0;
          s_txm13_ff1 <= s_txm13_ff0;
          
          if trans_i = '1' then
            s_txpre_count <= conv_unsigned(0,6);
          else
            if s_mode=conv_unsigned(0,2) then
              s_txpre_count <= s_txpre_count + conv_unsigned(1,1);
              if s_txpre_count = conv_unsigned(11,6) then
                s_txpre_count <= conv_unsigned(0,6);
              end if;
            elsif s_mode=conv_unsigned(2,2) then 
              s_txpre_count <= s_txpre_count + conv_unsigned(1,1);
            else
              if s_tf = '1' then
                s_txpre_count <= s_txpre_count + conv_unsigned(1,1);  
              end if;
            end if;              
          end if;

          if s_detect = '1' then
            s_rxpre_count <= conv_unsigned(0,6);
          else
            if s_mode=conv_unsigned(0,2) then
              s_rxpre_count <= s_rxpre_count + conv_unsigned(1,1);
              if s_rxpre_count = conv_unsigned(11,6) then
                s_rxpre_count <= conv_unsigned(0,6);
              end if;
            elsif s_mode=conv_unsigned(2,2) then 
              s_rxpre_count <= s_rxpre_count + conv_unsigned(1,1);
            else
              if s_tf = '1' then
                s_rxpre_count <= s_rxpre_count + conv_unsigned(1,1);  
              end if;
            end if;              
          end if;

          if smod_i = '1' then
            if s_rxpre_count(3 downto 0) = conv_unsigned(15,4) then
              s_rxm13_ff0 <= '1';
            else
              s_rxm13_ff0 <= '0';
            end if;
          else
            if s_rxpre_count(4 downto 0) = conv_unsigned(31,5) then
              s_rxm13_ff0 <= '1';
            else
              s_rxm13_ff0 <= '0';
            end if;              
          end if;
             
          if smod_i = '1' then
            if s_txpre_count(3 downto 0) = conv_unsigned(15,4) then
              s_txm13_ff0 <= '1';
            else
              s_txm13_ff0 <= '0';
            end if;
          else
            if s_txpre_count(4 downto 0) = conv_unsigned(31,5) then
              s_txm13_ff0 <= '1';
            else
              s_txm13_ff0 <= '0';
            end if;              
          end if;
            
        end if;
      end if;    

  end process p_divide_clk;

-------------------------------------------------------------------------------
  -- This section samples the serial input for data detection, that is a
  -- 1-to-0 transition at rxd in state "0000".
  -- In all other states this unit reads the data bits depending on the baud
  -- rate. In mode0 this section is not active.

  s_detect <= '1' when s_det_ff0 = '0' and s_det_ff1 = '1' else '0'; 
  s_rxd_val <= '1' when (s_rxd_ff0 = '1' and s_rxd_ff1 = '1') or
                        (s_rxd_ff0 = '1' and s_rxd_ff2 = '1') or
                        (s_rxd_ff1 = '1' and s_rxd_ff2 = '1') else '0';
               
  p_sample_rx: process (clk, cen, reset)
  begin
    if reset = '1' then
      s_rxd_ff0 <= '0';
      s_rxd_ff1 <= '0';
      s_rxd_ff2 <= '0';
      s_det_ff0 <= '0';
      s_det_ff1 <= '0';
    else
      if clk'event and clk='1' and cen='1' then          
        if s_recv_state = conv_unsigned(0,4) then   -- state "0000" means
          if s_ren = '1' then                       -- to listen for a 1 to 0
            case s_mode is                          -- transition
              when ("01") | ("11") => 
                if smod_i = '1' then
                  if s_tf = '1' then
                    s_det_ff0 <= rxd_i;  
                    s_det_ff1 <= s_det_ff0;
                  end if;
                else
                  if s_rxpre_count(0) = '1' then
                    s_det_ff0 <= rxd_i;  
                    s_det_ff1 <= s_det_ff0;                    
                  end if;
                end if;
              when ("10") =>
                if smod_i = '1' then
                  if s_rxpre_count(0) = '1' then
                    s_det_ff0 <= rxd_i;  
                    s_det_ff1 <= s_det_ff0;
                  end if;
                else
                  if s_rxpre_count(1) = '1' then
                    s_det_ff0 <= rxd_i;  
                    s_det_ff1 <= s_det_ff0;                    
                  end if;
                end if;
              when others =>
                null;
            end case;
          else
            s_det_ff0 <= '0';
            s_det_ff1 <= '0';              
          end if;
        else                                        -- in all other states
          s_det_ff0 <= '0';
          s_det_ff1 <= '0';
          if s_ren = '1' then                       -- sample for data bits
            case s_mode is
              when ("01") | ("11") => 
                if smod_i = '1' then
                  if s_rxpre_count(3 downto 0) = conv_unsigned(7,4) or
                     s_rxpre_count(3 downto 0) = conv_unsigned(8,4) or
                     s_rxpre_count(3 downto 0) = conv_unsigned(9,4) then
                    s_rxd_ff0 <= rxd_i;  
                    s_rxd_ff1 <= s_rxd_ff0;
                    s_rxd_ff2 <= s_rxd_ff1;
                  end if;                
                else
                  if s_rxpre_count(4 downto 0) = conv_unsigned(14,5) or
                     s_rxpre_count(4 downto 0) = conv_unsigned(16,5) or
                     s_rxpre_count(4 downto 0) = conv_unsigned(18,5) then 
                    s_rxd_ff0 <= rxd_i;  
                    s_rxd_ff1 <= s_rxd_ff0;                    
                    s_rxd_ff2 <= s_rxd_ff1;
                  end if;
                end if;
              when ("10") =>
                if smod_i = '1' then
                  if s_rxpre_count(4 downto 0) = conv_unsigned(14,5) or
                     s_rxpre_count(4 downto 0) = conv_unsigned(16,5) or
                     s_rxpre_count(4 downto 0) = conv_unsigned(18,5) then 
                    s_rxd_ff0 <= rxd_i;  
                    s_rxd_ff1 <= s_rxd_ff0;
                    s_rxd_ff2 <= s_rxd_ff1;
                  end if;
                else
                  if s_rxpre_count(5 downto 0) = conv_unsigned(28,6) or
                     s_rxpre_count(5 downto 0) = conv_unsigned(32,6) or
                     s_rxpre_count(5 downto 0) = conv_unsigned(36,6) then
                    s_rxd_ff0 <= rxd_i;  
                    s_rxd_ff1 <= s_rxd_ff0;                    
                    s_rxd_ff2 <= s_rxd_ff1;
                  end if;
                end if;
              when others =>
                null;
            end case;
          end if;
        end if;
      end if;
    end if;

  end process p_sample_rx;


-------------------------------------------------------------------------------
--*************************** TRANSMIT ****************************************
-- This is the finit state machine for the transmit shift register
-------------------------------------------------------------------------------

    txd_o <= s_txdm0;
  
  p_transmit : process (clk, cen, reset)

    variable v_txstep : std_logic_vector(1 downto 0);
    
  begin
    
    if reset = '1' then
      s_tran_state <= conv_unsigned(0, 4);
      s_tran_sh    <= conv_unsigned(0, 11);
      s_tran_done  <= '0';

      s_txdm0 <= '1';
      rxd_o   <= '1';
      rxdwr_o <= '0';
    else
      if clk'event and clk = '1' and cen='1' then

        -- Set default behavior
        v_txstep := "00";

        case s_mode is
-------------------------------------------------------------------------------
-- MODE 0
-------------------------------------------------------------------------------
          when ("00") =>
	    
            if s_tran_state = conv_unsigned(1, 4) or
              s_tran_state = conv_unsigned(2, 4) or
              s_tran_state = conv_unsigned(3, 4) or
              s_tran_state = conv_unsigned(4, 4) or
              s_tran_state = conv_unsigned(5, 4) or
              s_tran_state = conv_unsigned(6, 4) or
              s_tran_state = conv_unsigned(7, 4) or
              s_tran_state = conv_unsigned(8, 4) or
              s_recv_state = conv_unsigned(1, 4) or
              s_recv_state = conv_unsigned(2, 4) or
              s_recv_state = conv_unsigned(3, 4) or
              s_recv_state = conv_unsigned(4, 4) or
              s_recv_state = conv_unsigned(5, 4) or
              s_recv_state = conv_unsigned(6, 4) or
              s_recv_state = conv_unsigned(7, 4) or
              s_recv_state = conv_unsigned(8, 4) then
              if s_txpre_count(3 downto 0) = conv_unsigned(14, 4) or
                s_txpre_count(3 downto 0) = conv_unsigned(6, 4) then
                s_txdm0 <= not(s_txdm0);
              end if;
            else
              s_txdm0 <= '1';
            end if;

            if s_m0_shift_en = '1' then
              case s_tran_state is
                when ("0001") =>  	-- D1
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  rxd_o                 <= s_tran_sh(1);
                  rxdwr_o <= '1';
                when ("0010") =>  	-- D2
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  rxd_o                 <= s_tran_sh(1);
                  rxdwr_o <= '1';
                when ("0011") =>  	-- D3
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  rxd_o                 <= s_tran_sh(1);
                  rxdwr_o <= '1';
                when ("0100") =>  	-- D4
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  rxd_o                 <= s_tran_sh(1);
                  rxdwr_o <= '1';
                when ("0101") =>  	-- D5
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  rxd_o                 <= s_tran_sh(1);
                  rxdwr_o <= '1';
                when ("0110") =>  	-- D6
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  rxd_o                 <= s_tran_sh(1);
                  rxdwr_o <= '1';
                when ("0111") =>  	-- D7
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  rxd_o                 <= s_tran_sh(1);
                  rxdwr_o <= '1';
                when ("1000") =>  	-- D8, STOP BIT
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  s_tran_done           <= '1';
                  v_txstep              := "10";
                  rxd_o                 <= s_tran_sh(1);
                  rxdwr_o <= '1';
                when others =>  	-- D0
                  -- commence transmission if conditions are met
                  rxdwr_o <= '0';
                  if s_trans = '1' then
                    s_tran_sh(10 downto 8) <= conv_unsigned(7, 3);
                    s_tran_sh(7 downto 0)  <= unsigned(sbuf_i);
                    v_txstep               := "01";
                    s_tran_done            <= '0';
                    rxd_o                  <= sbuf_i(0);
                    rxdwr_o <= '1';
                  end if;
              end case;
            end if;
-------------------------------------------------------------------------------
-- MODE 1
-------------------------------------------------------------------------------
          when ("01") =>
            rxdwr_o <= '0';
            rxd_o <= '0';
            case s_tran_state is
              when ("0001") =>  	-- D1
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0010") =>  	-- D2
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0011") =>  	-- D3
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0100") =>  	-- D4
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0101") =>  	-- D5
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0110") =>  	-- D6
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0111") =>  	-- D7
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("1000") =>  	-- D8
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("1001") =>  	-- D9, set done bit
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  s_tran_done           <= '1';
                  v_txstep              := "10";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when others =>  		-- D0
                -- commence transmission if conditions are met
                s_txdm0 <= '1';
                if s_m13_txshift_en = '1' then
                  if s_trans = '1' then
                    s_tran_sh(10 downto 9) <= conv_unsigned(3, 2);
                    s_tran_sh(8 downto 1)  <= unsigned(sbuf_i);
                    s_tran_sh(0)           <= '0';
                    v_txstep               := "01";
                    s_tran_done            <= '0';
                    s_txdm0                <= '0';
                  end if;
                end if;
            end case;
-------------------------------------------------------------------------------
-- MODE 2
-------------------------------------------------------------------------------
          when ("10") =>
            rxdwr_o <= '0';
            rxd_o <= '0';
            case s_tran_state is
              when ("0001") =>  	-- D1
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0010") =>  	-- D2
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0011") =>  	-- D3
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0100") =>  	-- D4
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0101") =>  	-- D5
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0110") =>  	-- D6
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0111") =>  	-- D7
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("1000") =>  	-- D8
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("1001") =>  	-- D9
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("1010") =>  	-- D10, set done bit
                if s_m2_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  s_tran_done           <= '1';
                  v_txstep              := "10";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when others =>  		-- D0
                -- commence transmission if conditions are met
                s_txdm0 <= '1';
                if s_m2_txshift_en = '1' then
                  if s_trans = '1' then
                    s_tran_sh(10)         <= '1';
                    s_tran_sh(9)          <= s_tb8;
                    s_tran_sh(8 downto 1) <= unsigned(sbuf_i);
                    s_tran_sh(0)          <= '0';
                    v_txstep              := "01";
                    s_tran_done           <= '0';
                    s_txdm0               <= '0';
                  end if;
                end if;
            end case;
-------------------------------------------------------------------------------
-- MODE 3
-------------------------------------------------------------------------------
          when ("11") =>
            rxd_o <= '0';
            rxdwr_o <= '0';
            case s_tran_state is
              when ("0001") =>  	-- D1
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0010") =>  	-- D2
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0011") =>  	-- D3
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0100") =>  	-- D4
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0101") =>  	-- D5
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0110") =>  	-- D6
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("0111") =>  	-- D7
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("1000") =>  	-- D8
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("1001") =>  	-- D9
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  v_txstep              := "01";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when ("1010") =>  	-- D10, set done bit
                if s_m13_txshift_en = '1' then
                  s_tran_sh(10)         <= '1';
                  s_tran_sh(9 downto 0) <= s_tran_sh(10 downto 1);
                  s_tran_done           <= '1';
                  v_txstep              := "10";
                  s_txdm0               <= s_tran_sh(1);
                end if;
              when others =>  		-- D0
                -- commence transmission if conditions are met
                s_txdm0 <= '1';
                if s_m13_txshift_en = '1' then
                  if s_trans = '1' then
                    s_tran_sh(10)         <= '1';
                    s_tran_sh(9)          <= s_tb8;
                    s_tran_sh(8 downto 1) <= unsigned(sbuf_i);
                    s_tran_sh(0)          <= '0';
                    s_tran_done           <= '0';
                    v_txstep              := "01";
                    s_txdm0               <= '0';
                  end if;
                end if;
            end case;
-------------------------------------------------------------------------------
          when others =>
            null;
        end case;

        case v_txstep is
          when "01" =>
            s_tran_state <= s_tran_state + conv_unsigned(1, 1);
          when "10" =>
            s_tran_state <= conv_unsigned(0, 4);
          when others =>
            null;
        end case;
        
      end if;
    end if;
  end process p_transmit;

-------------------------------------------------------------------------------
--**************************** RECEIVE ****************************************
-- This is the finit state machine for the receive shift register
-------------------------------------------------------------------------------
      
  p_receive: process (clk, cen, reset)    
    variable v_rxstep : std_logic_vector(1 downto 0);
    
    begin

      if reset = '1' then
        s_recv_state <= conv_unsigned(0,4);
        s_recv_sh <= conv_unsigned(0,8);
        s_recv_buf <= conv_unsigned(0,8);
        s_recv_done <= '0';
        s_rb8 <= '0';
      else
        if clk'event and clk = '1' and cen='1' then

-------------------------------------------------------------------------------
-- MODE 0
-------------------------------------------------------------------------------
	  v_rxstep := "00";
          case s_mode is
          when ("00") =>
            case s_recv_state is
            when ("0000") =>            -- D0
              -- commence reception if conditions are met
              if s_ren = '1' and s_ri = '0' then
                if s_m0_shift_en = '1' then
                  v_rxstep := "01";
                  s_recv_done <= '0';
                end if;
              end if;
            when ("0001") =>            -- D1
              if s_m0_shift_en = '1' then
                s_recv_sh(7) <= rxd_i;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0010") =>            -- D2
              if s_m0_shift_en = '1' then
                s_recv_sh(7) <= rxd_i;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0011") =>            -- D3
              if s_m0_shift_en = '1' then
                s_recv_sh(7) <= rxd_i;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0100") =>            -- D4
              if s_m0_shift_en = '1' then
                s_recv_sh(7) <= rxd_i;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0101") =>            -- D5
              if s_m0_shift_en = '1' then
                s_recv_sh(7) <= rxd_i;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0110") =>            -- D6
              if s_m0_shift_en = '1' then
                s_recv_sh(7) <= rxd_i;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0111") =>            -- D6
              if s_m0_shift_en = '1' then
                s_recv_sh(7) <= rxd_i;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1000") =>            -- D7, set bits and store data
              if s_m0_shift_en = '1' then
                s_recv_sh(7) <= rxd_i;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                s_recv_done <= '1';
                s_recv_buf(7) <= rxd_i;
		s_recv_buf(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "10";
              end if;
            when others =>            
                v_rxstep := "10";
            end case;

-------------------------------------------------------------------------------
-- MODE 1
-------------------------------------------------------------------------------
          when ("01") =>
            case s_recv_state is
            when ("0000") =>            -- synchronise reception
              if s_ren = '1' and s_detect = '1' then
                v_rxstep := "01";
                s_recv_sh <= conv_unsigned(0,8);
                s_recv_done <= '0';
              end if;
            when ("0001") =>            -- D0 = START BIT
              if s_detect = '0' then
                if s_rxd_val = '0' then
                  if s_m13_rxshift_en = '1' then
                    s_recv_sh(7) <= s_rxd_val;
		    s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
                  end if;
                else                    -- reject false start bits
                  if s_m13_rxshift_en = '1' then
                v_rxstep := "10";                    
                  end if;
                end if;                  
              end if;
            when ("0010") =>            -- D1
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0011") =>            -- D2
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0100") =>            -- D3
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0101") =>            -- D4
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0110") =>            -- D5
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0111") =>            -- D6
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1000") =>            -- D7
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1001") =>            -- D8
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1010") =>            -- D9 = STOP BIT
              -- store data and set interrupt bit if conditions are met.
              if (s_ri = '0' and s_sm2 = '0') or
                 (s_ri = '0' and s_rxd_val = '1') then
                --if s_m13_rxshift_en = '1' then
                if s_rxpre_count(3 downto 0) = conv_unsigned(10,4) then      -- CK, CV: changed to enter state 0 after one stopbit
                  s_recv_sh(7) <= s_rxd_val;
		      s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                  v_rxstep := "10";
                  s_recv_done <= '1';
                  s_rb8 <= s_rxd_val;
                  s_recv_buf <= s_recv_sh(7 downto 0);
                end if;
              -- forget data and recommence listening for a start bit
              else
                if s_m13_rxshift_en = '1' then
                v_rxstep := "10";                  
                end if;                
              end if;
            when others =>
                v_rxstep := "10";
            end case;

-------------------------------------------------------------------------------
-- MODE 2
-------------------------------------------------------------------------------
          when ("10") =>
            case s_recv_state is
            when ("0000") =>            -- synchronise reception
              if s_ren = '1' and s_detect = '1' then
                v_rxstep := "01";
                s_recv_sh <= conv_unsigned(0,8);
                s_recv_done <= '0';
              end if;
            when ("0001") =>            -- D0 = START BIT
              if s_rxd_val = '0' then
                if s_m2_rxshift_en = '1' then
                  s_recv_sh(7) <= s_rxd_val;
		  s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
                end if;
              else                    -- reject false start bits
                if s_m2_rxshift_en = '1' then
                v_rxstep := "10";                    
                end if;
              end if;
            when ("0010") =>            -- D1
              if s_m2_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0011") =>            -- D2
              if s_m2_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0100") =>            -- D3
              if s_m2_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0101") =>            -- D4
              if s_m2_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0110") =>            -- D5
              if s_m2_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0111") =>            -- D6
              if s_m2_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1000") =>            -- D7
              if s_m2_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1001") =>            -- D8
              if s_m2_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1010") =>            -- D9
              -- store data and set interrupt bit if conditions are met.
              if (s_ri = '0' and s_sm2 = '0') or
                 (s_ri = '0' and s_rxd_val = '1') then
                if s_m2_rxshift_en = '1' then
                  s_recv_sh(7) <= s_rxd_val;
		  s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                  s_recv_done <= '1';
                  s_rb8 <= s_rxd_val;
                  s_recv_buf <= s_recv_sh(7 downto 0);
                end if;
              end if;
              -- forget data 
              if s_m2_rxshift_en = '1' then
                v_rxstep := "01";
              end if;                
            when ("1011") =>            -- D10 STOP BIT
              -- recommence listening for a start bit
              if s_m2_rxshift_en = '1' then
                -- s_recv_state <= conv_unsigned(0,4);
                v_rxstep := "10";
              end if;
            when others =>
              -- s_recv_state <= conv_unsigned(0,4);
                v_rxstep := "10";
            end case;

-------------------------------------------------------------------------------
-- MODE 3
-------------------------------------------------------------------------------
          when ("11") =>
            case s_recv_state is
            when ("0000") =>            -- synchronise reception
              if s_ren = '1' and s_detect = '1' then
                v_rxstep := "01";
                s_recv_sh <= conv_unsigned(0,8);
                s_recv_done <= '0';
              end if;
            when ("0001") =>            -- D0 = START BIT
              if s_detect = '0' then
                if s_rxd_val = '0' then
                  if s_m13_rxshift_en = '1' then
                    s_recv_sh(7) <= s_rxd_val;
	            s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                  v_rxstep := "01";
                  end if;
                else                    -- reject false start bits
                  if s_m13_rxshift_en = '1' then
                  v_rxstep := "10";                    
                  end if;
                end if;
              end if;
            when ("0010") =>            -- D1
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0011") =>            -- D2
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0100") =>            -- D3
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0101") =>            -- D4
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0110") =>            -- D5
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("0111") =>            -- D6
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1000") =>            -- D7
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1001") =>            -- D8
              if s_m13_rxshift_en = '1' then
                s_recv_sh(7) <= s_rxd_val;
		s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                v_rxstep := "01";
              end if;
            when ("1010") =>            -- D9
              -- store data and set interrupt bit if conditions are met.
              if (s_ri = '0' and s_sm2 = '0') or
                 (s_ri = '0' and s_rxd_val = '1') then
                if s_m13_rxshift_en = '1' then
                  s_recv_sh(7) <= s_rxd_val;
		  s_recv_sh(6 downto 0) <= s_recv_sh(7 downto 1);
                  s_recv_done <= '1';
                  s_rb8 <= s_rxd_val;
                  s_recv_buf <= s_recv_sh(7 downto 0);
                end if;
              end if;
              -- forget data 
              if s_m13_rxshift_en = '1' then
                v_rxstep := "01";
              end if;                
            when ("1011") =>            -- D10 STOP BIT
              -- recommence listening for a start bit
              if s_m13_rxshift_en = '1' then
                v_rxstep := "10";
              end if;
            when others =>
                v_rxstep := "10";
            end case;
          when others =>
            null;
          end case;

	  case v_rxstep is
	    when "01" =>
	      s_recv_state <= s_recv_state + conv_unsigned(1,1);
	    when "10" =>
	      s_recv_state <= conv_unsigned(0,4);
	    when others =>
	      null;
	  end case;
	  
        end if;
      end if;


  end process p_receive;

  
end rtl;
