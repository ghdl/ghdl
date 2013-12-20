
-- Copyright (C) 2002 Morgan Kaufmann Publishers, Inc

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

library ieee_proposed;
use ieee_proposed.electrical_systems.all;
use ieee_proposed.mechanical_systems.all;
                        
entity inline_20a is

end entity inline_20a;


architecture test of inline_20a is

  signal trigger, discharge, clk : bit;
  constant capacitance : real := 1.0e-9;

begin


  block_1 : block is

    terminal cap : electrical;
    quantity v_cap across i_cap through cap;
    
  begin

    -- code from book
  
    i_cap == capacitance * v_cap'dot;

    --

    trigger_reset : process (trigger) is
    begin
      if trigger = '1' then
        break v_cap => 0.0;
      end if;
    end process trigger_reset;

    -- end code from book

  end block block_1;


  block_2 : block is

    constant mass : real := 1.0;
    terminal n : translational_v;
    quantity v across n;
    quantity applied_force : real;
    quantity acceleration : real;

    quantity vx, vy : real;

  begin

    acceleration == v'dot;
    
    -- code from book

    applied_force == mass * acceleration;

    -- end code from book

    process is
    begin

      -- code from book

      break acceleration'integ => - acceleration'integ;

      --

      break vx => 0.0, vy => 0.0;
      
      -- end code from book

      wait;
    end process;

  end block block_2;


  block_3 : block is

    terminal cap : electrical;
    quantity v_cap across i_cap through cap;
    
  begin

    i_cap == capacitance * v_cap'dot;

    -- code from book

    trigger_reset : process (trigger) is
    begin
      break v_cap => 0.0 when trigger = '1';
    end process trigger_reset;

    -- end code from book

  end block block_3;


  block_4 : block is

    terminal cap : electrical;
    quantity v_cap across i_cap through cap;
    quantity charge : real;
    
  begin

    -- code from book

    charge == capacitance * v_cap;
    
    i_cap == charge'dot;

    --
    
    trigger_reset : process (trigger) is
    begin
      if trigger = '1' then
        break for charge use v_cap => 0.0;
      end if;
    end process trigger_reset;

    -- end code from book

  end block block_4;


  block_5 : block is

    terminal cap : electrical;
    quantity v_cap across i_cap through cap;
    quantity charge : real;
    
  begin

    charge == capacitance * v_cap;
    i_cap == charge'dot;

    -- code from book

    trigger_reset : process (trigger) is
    begin
      break for charge use v_cap => 0.0 when trigger = '1';
    end process trigger_reset;

    -- end code from book

  end block block_5;


  block_6 : block is

    terminal cap : electrical;
    quantity v_cap across i_cap through cap;
    quantity cap_charge : real;
    
  begin

    cap_charge == capacitance * v_cap;
    i_cap == cap_charge'dot;

    -- code from book

    discharge_cap : break cap_charge => 0.0
                      on clk when discharge = '1' and clk = '1';

    -- end code from book

  end block block_6;


  block_7 : block is

    terminal cap : electrical;
    quantity v_cap across i_cap through cap;
    quantity cap_charge : real;
    
  begin

    cap_charge == capacitance * v_cap;
    i_cap == cap_charge'dot;

    -- code from book

    discharge_cap : process is
    begin
      break cap_charge => 0.0 when discharge = '1' and clk = '1';
      wait on clk;
    end process discharge_cap;

    -- end code from book

  end block block_7;


  block_8 : block is

    terminal cap : electrical;
    quantity v_cap across i_cap through cap;
    quantity charge : real;
    
  begin

    charge == capacitance * v_cap;
    i_cap == charge'dot;

    -- code from book

    trigger_reset : break for charge use v_cap => 0.0 when trigger = '1';

    -- end code from book

  end block block_8;


  block_9 : block is

    terminal cap : electrical;
    quantity v_cap across i_cap through cap;
    quantity charge : real;
    
  begin

    charge == capacitance * v_cap;
    i_cap == charge'dot;

    -- code from book

    trigger_reset : process is
    begin
      break for charge use v_cap => 0.0 when trigger = '1';
      wait on trigger;
    end process trigger_reset;

    -- end code from book

  end block block_9;


  block_10 : block is

    quantity q : real;
    constant new_q : real := 0.0;
    
  begin

    -- code from book

    useless_break : break q => new_q when q < 0.0 or q > 3.0;

    -- end code from book

  end block block_10;


  block_11 : block is

    quantity q : real;
    constant new_q : real := 0.0;
    
  begin

    -- code from book

    useless_break : process is
    begin
      break q => new_q when q < 0.0 or q > 3.0;
      wait;
    end process useless_break;

    -- end code from book

  end block block_11;


  block_12 : block is

    quantity q : real;
    constant new_q : real := 0.0;
    
  begin

    -- code from book

    correct_break : break q => new_q on q'above(0.0), q'above(3.0)
      when q < 0.0 or q > 3.0;

    -- end code from book

  end block block_12;


end architecture test;
