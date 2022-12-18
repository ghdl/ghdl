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
--         Filename:               mc8051_tmrctr_rtl.vhd
--
--         Date of Creation:       Mon Aug  9 12:14:48 1999
--
--         Version:                $Revision: 1.9 $
--
--         Date of Latest Version: $Date: 2006-09-07 09:43:21 $
--
--
--         Description: Timer/Counter unit of the mc8051 microcontroller.
--
--
--
--
-------------------------------------------------------------------------------
architecture rtl of mc8051_tmrctr is

  signal s_pre_count    : unsigned(3 downto 0);  -- these two signals provide
  signal s_count_enable : std_logic;             -- a clock enable signal which
                                                 -- masks out every twelfth
                                                 -- rising edge of clk
  
  signal s_count0       : unsigned(15 downto 0); -- count for tmr/ctr0
  signal s_countl0      : unsigned(7 downto 0);  -- count register for tmr/ctr0
  signal s_counth0      : unsigned(7 downto 0);  -- count register for tmr/ctr0
  signal s_count1       : unsigned(15 downto 0); -- count for tmr/ctr1
  signal s_countl1      : unsigned(7 downto 0);  -- count register for tmr/ctr1
  signal s_counth1      : unsigned(7 downto 0);  -- count register for tmr/ctr1
  signal s_gate0        : std_logic;             -- gate bit for tmr/ctr 0
  signal s_gate1        : std_logic;             -- gate bit for tmr/ctr 1
  signal s_c_t0         : std_logic;             -- tmr/ctr 0 is timer if 0
  signal s_c_t1         : std_logic;             -- tmr/ctr 1 is timer if 0
  signal s_tmr_ctr0_en  : std_logic;             -- starts tmr/ctr 0 if 1
  signal s_tmr_ctr1_en  : std_logic;             -- starts tmr/ctr 1 if 1
  signal s_mode0        : unsigned(1 downto 0);  -- mode of tmr/ctr 0
  signal s_mode1        : unsigned(1 downto 0);  -- mode of tmr/ctr 1
  signal s_tf0          : std_logic;             -- overflow flag of tmr/ctr 0
  signal s_tf1          : std_logic;             -- overflow flag of tmr/ctr 1
  signal s_t0ff0        : std_logic;             -- flipflop for edge dedection
  signal s_t0ff1        : std_logic;             -- flipflop for edge dedection
  signal s_t0ff2        : std_logic;             -- flipflop for edge dedection
  signal s_t1ff0        : std_logic;             -- flipflop for edge dedection
  signal s_t1ff1        : std_logic;             -- flipflop for edge dedection
  signal s_t1ff2        : std_logic;             -- flipflop for edge dedection
  signal s_ext_edge0    : std_logic;             -- 1 if external edge dedected
  signal s_ext_edge1    : std_logic;             -- 1 if external edge dedected
  signal s_int0_sync    : std_logic_vector(1 downto 0); -- sync int0 input
  signal s_int1_sync    : std_logic_vector(1 downto 0); -- sync int1 input
  
  
begin                 -- architecture rtl

  -- The names of the following signals make the code more readable.
  s_gate0 <= tmod_i(3);
  s_c_t0  <= tmod_i(2);
  s_mode0(1) <= tmod_i(1);
  s_mode0(0) <= tmod_i(0);
  
  s_gate1 <= tmod_i(7);
  s_c_t1  <= tmod_i(6);
  s_mode1(1) <= tmod_i(5);
  s_mode1(0) <= tmod_i(4);

  -- These two signals start the corresponding timer/counter if they are 1.
  s_tmr_ctr0_en <= tcon_tr0_i and (not(s_gate0) or s_int0_sync(1));
  s_tmr_ctr1_en <= (not(s_gate1) or s_int1_sync(1)) when
                                              s_mode0 = conv_unsigned(3,2) else
                   tcon_tr1_i and (not(s_gate1) or s_int1_sync(1));

  -- The outputs of this unit are the two timer overflow flags, which are read
  -- by the control unit and the two 16 bit count registers to enable read
  -- access.
  tf0_o <= s_tf0;
  tf1_o <= s_tf1;
  th0_o <= std_logic_vector(s_count0(15 downto 8));
  tl0_o <= std_logic_vector(s_count0(7 downto 0));
  th1_o <= std_logic_vector(s_count1(15 downto 8));
  tl1_o <= std_logic_vector(s_count1(7 downto 0));

-------------------------------------------------------------------------------
  -- The register s_pre_count is driven with the system clock. So a
  -- good enable signal (which is stable when clk has its rising edge) can be
  -- derived to mask out every twelfth pulse of clk. Also synchronize the
  -- interrupt inputs here.

  s_count_enable <= '1' when s_pre_count = conv_unsigned(11,4) else '0';
  
  p_divide_clk: process (clk, cen, reset)
    
    begin

      if reset = '1' then
        s_pre_count <= conv_unsigned(0,4);
        s_int0_sync <= "00";
        s_int1_sync <= "00";
      else
        if clk'event and clk='1' and cen='1' then
          s_pre_count <= s_pre_count + conv_unsigned(1,1);
          if s_pre_count = conv_unsigned(11,4) then
            s_pre_count <= conv_unsigned(0,4);
          end if;
          s_int0_sync(0) <= int0_i;
          s_int0_sync(1) <= s_int0_sync(0);
          s_int1_sync(0) <= int1_i;
          s_int1_sync(1) <= s_int1_sync(0);
        end if;
      end if;    

  end process p_divide_clk;

-------------------------------------------------------------------------------
  -- The two flip flops are updated every second clock period.
  -- If a falling edge
  -- on the port t0_i is dedected the signal s_ext_edge0 is set to 1 and with
  -- the next rising edge of clk the counter0 is incremented.
  -- The same function is realised for counter1.
  s_ext_edge0 <= '1' when (s_t0ff1 = '0' and s_t0ff2 = '1') else '0';      

  p_sample_t0: process (clk, cen, reset)
      
    begin

      if reset = '1' then
        s_t0ff0 <= '0';
        s_t0ff1 <= '0';
        s_t0ff2 <= '0';
      else
            
        if clk'event and clk = '1' and cen='1' then
          if s_pre_count = conv_unsigned(6,3) then
            if s_c_t0 = '1' then
              s_t0ff0 <= t0_i;
              s_t0ff1 <= s_t0ff0;
              s_t0ff2 <= s_t0ff1;
            end if;
          end if;
        end if;  
      end if;    

  end process p_sample_t0;
      
  s_ext_edge1 <= '1' when (s_t1ff1 = '0' and s_t1ff2 = '1') else '0';

  p_sample_t1: process (clk, cen, reset)
      
    begin

      if reset = '1' then
        s_t1ff0 <= '0';
        s_t1ff1 <= '0';
        s_t1ff2 <= '0';
      else
        if clk'event and clk = '1' and cen='1' then
          if s_pre_count = conv_unsigned(6,3) then
            if s_c_t1 = '1' then
              s_t1ff0 <= t1_i;
              s_t1ff1 <= s_t1ff0;
              s_t1ff2 <= s_t1ff1;
            end if;              
          end if;
        end if;  
      end if;    

  end process p_sample_t1;

------------------------------------------------------------------------------
--+++++++++++++++++++++   TIMER / COUNTER 0   ++++++++++++++++++++++++++++++--
------------------------------------------------------------------------------
-- This is timer/counter0. It is built around the 16 bit count register
-- s_count0 and realises its four operating modes
------------------------------------------------------------------------------
  s_count0(15 downto 8) <= s_counth0;
  s_count0(7 downto 0) <= s_countl0;
  s_count1(15 downto 8) <= s_counth1;
  s_count1(7 downto 0) <= s_countl1;
      
  p_tmr_ctr: process (clk, cen, reset)
    
  begin

    if reset = '1' then                 -- perform asynchronous reset

      s_countl0 <= conv_unsigned(0,8);
      s_counth0 <= conv_unsigned(0,8);
      s_countl1 <= conv_unsigned(0,8);
      s_counth1 <= conv_unsigned(0,8);
      s_tf1    <= '0';
      s_tf0    <= '0';
        
    else
        
      if clk'event and clk = '1' and cen='1' then
      s_tf1    <= '0';
      s_tf0    <= '0';
          
-------------------------------------------------------------------------------
-- operating mode 0 (13 bit timer/counter)
-------------------------------------------------------------------------------
      case s_mode0 is
        when "00" =>

        -- This section generates the timer/counter overflow flag0
        if s_tmr_ctr0_en = '1' then
          if s_count_enable = '1' then   
            if s_c_t0 = '0' or (s_ext_edge0 = '1' and s_c_t0 = '1')  then
              if s_count0 = conv_unsigned(65311,16) then
                s_tf0 <= '1';
              else
                s_tf0 <= '0';
              end if;
            end if;
          end if;
        end if;
        
        -- This section generates the low byte register of tmr/ctr0
        if wt_i = "00" and wt_en_i = '1' then
          s_countl0 <= unsigned(reload_i);  
        else
          if s_tmr_ctr0_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t0 = '0' then
                if s_countl0 = conv_unsigned(31,8) then
                  s_countl0 <= conv_unsigned(0,8);
                else
                  s_countl0 <= s_countl0 + conv_unsigned(1,1);
                end if;
              else
                if s_ext_edge0 = '1' then
                  if s_countl0 = conv_unsigned(31,8) then
                    s_countl0 <= conv_unsigned(0,8);
                  else
                    s_countl0 <= s_countl0 + conv_unsigned(1,1);
                  end if;
                end if;                  
              end if;
            end if; 
          end if;
        end if;
        
        -- This section generates the high byte register of tmr/ctr0
        if wt_i = "10" and wt_en_i = '1' then
          s_counth0 <= unsigned(reload_i);  
        else
          if s_tmr_ctr0_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t0 = '0' then
                if s_count0 = conv_unsigned(65311,16) then
                  s_counth0 <= conv_unsigned(0,8);
                else
                  if s_countl0 = conv_unsigned(31,8) then
                    s_counth0 <= s_counth0 + conv_unsigned(1,1);
                  end if;
                end if;
              else
                if s_ext_edge0 = '1' then
                  if s_count0 = conv_unsigned(65311,16) then
                    s_counth0 <= conv_unsigned(0,8);
                  else
                    if s_countl0 = conv_unsigned(31,8) then
                      s_counth0 <= s_counth0 + conv_unsigned(1,1);
                    end if;
                  end if;
                end if;                  
              end if;
            end if;
          end if;
        end if;
-------------------------------------------------------------------------------
-- operating mode 1 (16 bit timer/counter)
-------------------------------------------------------------------------------

      when "01" =>

        -- This section generates the timer/counter overflow flag0
        if s_tmr_ctr0_en = '1' then
          if s_count_enable = '1' then   
            if s_c_t0 = '0' or (s_ext_edge0 = '1' and s_c_t0 = '1')  then
              if s_count0 = conv_unsigned(65535,16) then
                s_tf0 <= '1';
              else
                s_tf0 <= '0';
              end if;
            end if;
          end if;
        end if;
        
        -- This section generates the low byte register of tmr/ctr0
        if wt_i = "00" and wt_en_i = '1' then
          s_countl0 <= unsigned(reload_i);  
        else
          if s_tmr_ctr0_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t0 = '0' then
                if s_count0 = conv_unsigned(65535,16) then
                  s_countl0 <= conv_unsigned(0,8);
                else
                  s_countl0 <= s_countl0 + conv_unsigned(1,1);
                end if;
              else
                if s_ext_edge0 = '1' then
                  if s_count0 = conv_unsigned(65535,16) then
                    s_countl0 <= conv_unsigned(0,8);
                  else
                    s_countl0 <= s_countl0 + conv_unsigned(1,1);
                  end if;
                end if;                  
              end if;
            end if; 
          end if;
        end if;
        
        -- This section generates the high byte register of tmr/ctr0
        if wt_i = "10" and wt_en_i = '1' then
          s_counth0 <= unsigned(reload_i);  
        else
          if s_tmr_ctr0_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t0 = '0' then
                if s_count0 = conv_unsigned(65535,16) then
                  s_counth0 <= conv_unsigned(0,8);
                else
                  if s_countl0 = conv_unsigned(255,8) then
                    s_counth0 <= s_counth0 + conv_unsigned(1,1);
                  end if;
                end if;
              else
                if s_ext_edge0 = '1' then
                  if s_count0 = conv_unsigned(65535,16) then
                    s_counth0 <= conv_unsigned(0,8);
                  else
                    if s_countl0 = conv_unsigned(255,8) then
                      s_counth0 <= s_counth0 + conv_unsigned(1,1);
                    end if;
                  end if;
                end if;                  
              end if;
            end if;
          end if;
        end if;


-------------------------------------------------------------------------------
-- operating mode 2 (8 bit timer/counter, autoreloaded from high byte register)
-------------------------------------------------------------------------------

      when "10" =>
               
        -- This section generates the timer/counter overflow flag0
        if s_tmr_ctr0_en = '1' then
          if s_count_enable = '1' then   
            if s_c_t0 = '0' or (s_ext_edge0 = '1' and s_c_t0 = '1')  then
              if s_count0(7 downto 0) = conv_unsigned(255,16) then
                s_tf0 <= '1';
              else
                s_tf0 <= '0';
              end if;
            end if;
          end if;
        end if;
        
        -- This section generates the low byte register of tmr/ctr0
        if wt_i = "00" and wt_en_i = '1' then
          s_countl0 <= unsigned(reload_i);  
        else
          if s_tmr_ctr0_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t0 = '0' then
                if s_countl0 = conv_unsigned(255,8) then
                  s_countl0 <= s_counth0;
                else
                  s_countl0 <= s_countl0 + conv_unsigned(1,1);
                end if;
              else
                if s_ext_edge0 = '1' then
                  if s_countl0 = conv_unsigned(255,8) then
                    s_countl0 <= s_counth0;
                  else
                    s_countl0 <= s_countl0 + conv_unsigned(1,1);
                  end if;
                end if;                  
              end if;
            end if; 
          end if;
        end if;
        
        -- This section generates the high byte register of tmr/ctr0
        if wt_i = "10" and wt_en_i = '1' then
          s_counth0 <= unsigned(reload_i);
        end if;
        
-------------------------------------------------------------------------------
-- operating mode 3 (One 8 bit timer/counter and one 8 bit timer)
-------------------------------------------------------------------------------

      when "11" =>
           
        -- This section generates the timer/counter overflow flag0
        if s_tmr_ctr0_en = '1' then
          if s_count_enable = '1' then   
            if s_c_t0 = '0' or (s_ext_edge0 = '1' and s_c_t0 = '1')  then
              if s_count0(7 downto 0) = conv_unsigned(255,16) then
                s_tf0 <= '1';
              else
                s_tf0 <= '0';
              end if;
            end if;
          end if;
        end if;
        
        -- This section generates the low byte register of tmr/ctr0
        if wt_i = "00" and wt_en_i = '1' then
          s_countl0 <= unsigned(reload_i);  
        else
          if s_tmr_ctr0_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t0 = '0' then
                if s_countl0 = conv_unsigned(255,8) then
                  s_countl0 <= conv_unsigned(0,8);
                else
                  s_countl0 <= s_countl0 + conv_unsigned(1,1);
                end if;
              else
                if s_ext_edge0 = '1' then
                  if s_countl0 = conv_unsigned(255,8) then
                    s_countl0 <= conv_unsigned(0,8);
                  else
                    s_countl0 <= s_countl0 + conv_unsigned(1,1);
                  end if;
                end if;                  
              end if;
            end if; 
          end if;
        end if;
                   
        -- This section generates the timer/counter overflow flag1
        if tcon_tr1_i = '1' then
          if s_count_enable = '1' then   
            if s_count0(15 downto 8) = conv_unsigned(255,8) then
              s_tf1 <= '1';
            else
              s_tf1 <= '0';
            end if;
          end if;
        end if;
        
        -- This section generates the high byte register of tmr/ctr0
        if wt_i = "10" and wt_en_i = '1' then
          s_counth0 <= unsigned(reload_i);  
        else
          if tcon_tr1_i = '1' then
            if s_count_enable = '1' then   
              if s_counth0 = conv_unsigned(255,8) then
                s_counth0 <= conv_unsigned(0,8);
              else
                s_counth0 <= s_counth0 + conv_unsigned(1,1);
              end if;
            end if; 
          end if;
        end if;
        
      when others => null;
    end case;
  
------------------------------------------------------------------------------
--+++++++++++++++++   END OF TIMER / COUNTER 0   +++++++++++++++++++++++++++--
------------------------------------------------------------------------------

  
------------------------------------------------------------------------------
--+++++++++++++++++++++   TIMER / COUNTER 1   ++++++++++++++++++++++++++++++--
------------------------------------------------------------------------------
-- This is timer/counter1. It is built around the 16 bit count register
-- s_count1 and realises its four operating modes
------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- operating mode 0 (13 bit timer/counter)
-------------------------------------------------------------------------------
      case s_mode1 is
        when "00" =>

        -- This section generates the timer/counter overflow flag1
        if s_tmr_ctr1_en = '1' then
          if s_count_enable = '1' then
            if s_mode0 = conv_unsigned(1,2) or
               s_mode0 = conv_unsigned(0,2) or
               s_mode0 = conv_unsigned(2,2) then
              if s_c_t1 = '0' or (s_ext_edge1 = '1' and s_c_t1 = '1')  then
                if s_count1 = conv_unsigned(65311,16) then
                  s_tf1 <= '1';
                else
                  s_tf1 <= '0';
                end if;
              end if;
            else
              null;
            end if;
          end if;
        end if;
        
        -- This section generates the low byte register of tmr/ctr1
        if wt_i = "01" and wt_en_i = '1' then
          s_countl1 <= unsigned(reload_i);  
        else
          if s_tmr_ctr1_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t1 = '0' then
                if s_countl1 = conv_unsigned(31,8) then
                  s_countl1 <= conv_unsigned(0,8);
                else
                  s_countl1 <= s_countl1 + conv_unsigned(1,1);
                end if;
              else
                if s_ext_edge1 = '1' then
                  if s_countl1 = conv_unsigned(31,8) then
                    s_countl1 <= conv_unsigned(0,8);
                  else
                    s_countl1 <= s_countl1 + conv_unsigned(1,1);
                  end if;
                end if;                  
              end if;
            end if; 
          end if;
        end if;
        
        -- This section generates the high byte register of tmr/ctr1
        if wt_i = "11" and wt_en_i = '1' then
          s_counth1 <= unsigned(reload_i);  
        else
          if s_tmr_ctr1_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t1 = '0' then
                if s_count1 = conv_unsigned(65311,16) then
                  s_counth1 <= conv_unsigned(0,8);
                else
                  if s_countl1 = conv_unsigned(31,8) then
                    s_counth1 <= s_counth1 + conv_unsigned(1,1);
                  end if;
                end if;
              else
                if s_ext_edge1 = '1' then
                  if s_count1 = conv_unsigned(65311,16) then
                    s_counth1 <= conv_unsigned(0,8);
                  else
                    if s_countl1 = conv_unsigned(31,8) then
                      s_counth1 <= s_counth1 + conv_unsigned(1,1);
                    end if;
                  end if;
                end if;                  
              end if;
            end if;
          end if;
        end if;
-------------------------------------------------------------------------------
-- operating mode 1 (16 bit timer/counter)
-------------------------------------------------------------------------------

        when "01" =>

        -- This section generates the timer/counter overflow flag1
        if s_tmr_ctr1_en = '1' then
          if s_count_enable = '1' then
            if s_mode0 = conv_unsigned(1,2) or
               s_mode0 = conv_unsigned(0,2) or
               s_mode0 = conv_unsigned(2,2) then
              if s_c_t1 = '0' or (s_ext_edge1 = '1' and s_c_t1 = '1')  then
                if s_count1 = conv_unsigned(65535,16) then
                  s_tf1 <= '1';
                else
                  s_tf1 <= '0';
                end if;
              end if;
            else
              null;
            end if;
          end if;
        end if;
        
        -- This section generates the low byte register of tmr/ctr1
        if wt_i = "01" and wt_en_i = '1' then
          s_countl1 <= unsigned(reload_i);  
        else
          if s_tmr_ctr1_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t1 = '0' then
                if s_count1 = conv_unsigned(65535,16) then
                  s_countl1 <= conv_unsigned(0,8);
                else
                  s_countl1 <= s_countl1 + conv_unsigned(1,1);
                end if;
              else
                if s_ext_edge1 = '1' then
                  if s_count1 = conv_unsigned(65535,16) then
                    s_countl1 <= conv_unsigned(0,8);
                  else
                    s_countl1 <= s_countl1 + conv_unsigned(1,1);
                  end if;
                end if;                  
              end if;
            end if; 
          end if;
        end if;
        
        -- This section generates the high byte register of tmr/ctr1
        if wt_i = "11" and wt_en_i = '1' then
          s_counth1 <= unsigned(reload_i);  
        else
          if s_tmr_ctr1_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t1 = '0' then
                if s_count1 = conv_unsigned(65535,16) then
                  s_counth1 <= conv_unsigned(0,8);
                else
                  if s_countl1 = conv_unsigned(255,8) then
                    s_counth1 <= s_counth1 + conv_unsigned(1,1);
                  end if;
                end if;
              else
                if s_ext_edge1 = '1' then
                  if s_count1 = conv_unsigned(65535,16) then
                    s_counth1 <= conv_unsigned(0,8);
                  else
                    if s_countl1 = conv_unsigned(255,8) then
                      s_counth1 <= s_counth1 + conv_unsigned(1,1);
                    end if;
                  end if;
                end if;                  
              end if;
            end if;
          end if;
        end if;
        
-------------------------------------------------------------------------------
-- operating mode 2 (8 bit timer/counter, auto reloaded)
-------------------------------------------------------------------------------

        when "10" =>
               
        -- This section generates the timer/counter overflow flag1
        if s_tmr_ctr1_en = '1' then
          if s_count_enable = '1' then   
            if s_mode0 = conv_unsigned(1,2) or
               s_mode0 = conv_unsigned(0,2) or
               s_mode0 = conv_unsigned(2,2) then
              if s_c_t1 = '0' or (s_ext_edge1 = '1' and s_c_t1 = '1')  then
                if s_count1(7 downto 0) = conv_unsigned(255,16) then
                  s_tf1 <= '1';
                else
                  s_tf1 <= '0';
                end if;
              end if;
            else
              null;
            end if;
          end if;
        end if;
        
        -- This section generates the low byte register of tmr/ctr1
        if wt_i = "01" and wt_en_i = '1' then
          s_countl1 <= unsigned(reload_i);  
        else
          if s_tmr_ctr1_en = '1' then
            if s_count_enable = '1' then   
              if s_c_t1 = '0' then
                if s_countl1 = conv_unsigned(255,8) then
                  s_countl1 <= s_counth1;
                else
                  s_countl1 <= s_countl1 + conv_unsigned(1,1);
                end if;
              else
                if s_ext_edge1 = '1' then
                  if s_countl1 = conv_unsigned(255,8) then
                    s_countl1 <= s_counth1;
                  else
                    s_countl1 <= s_countl1 + conv_unsigned(1,1);
                  end if;
                end if;                  
              end if;
            end if; 
          end if;
        end if;
        
        -- This section generates the high byte register of tmr/ctr1
        if wt_i = "11" and wt_en_i = '1' then
          s_counth1 <= unsigned(reload_i);
        end if;
        
-------------------------------------------------------------------------------
-- operating mode 3 (One 8 bit timer/counter and one 8 bit timer)
-------------------------------------------------------------------------------

      when "11" =>
        
        -- This section generates the low byte register of tmr/ctr1
        if wt_i = "01" and wt_en_i = '1' then
          s_countl1 <= unsigned(reload_i);  
        end if;
        
        -- This section generates the high byte register of tmr/ctr1
        if wt_i = "11" and wt_en_i = '1' then
          s_counth1 <= unsigned(reload_i);
        end if;
          
      when others => null;
    end case;
------------------------------------------------------------------------------
--+++++++++++++++++   END OF TIMER / COUNTER 1   +++++++++++++++++++++++++++--
------------------------------------------------------------------------------
      
    end if;  
  end if;
  
  end process p_tmr_ctr;

  
end rtl;

