
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
-- $Id: ch_20_ch_20_08.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity ch_20_08 is

end entity ch_20_08;


----------------------------------------------------------------


library ieee;  use ieee.std_logic_1164.all;

architecture std_cell of ch_20_08 is

  attribute cell_name : string;
  attribute pin_number : positive;
  attribute max_wire_delay : delay_length;
  attribute encoding : bit_vector;

  type length is range 0 to integer'high
    units nm;
          um = 1000 nm;
          mm = 1000 um;
          mil = 25400 nm;
    end units length;

  type coordinate is record
                       x, y : length;
                     end record coordinate;

  attribute cell_position : coordinate;

  type built_in_type is (bv_incr, std_incr);
  attribute built_in : built_in_type;

  signal enable, clk : bit;

  type state_type is (idle_state, other_state);

  type speed_range is (high, other_speed);
  type coolant_level is (high, other_level);

  attribute representation : string;

  function increment ( vector : in bit_vector ) return bit_vector is
  begin
  end;

  function increment ( vector : in std_logic_vector ) return std_logic_vector is
  begin
  end;

  attribute cell_name of std_cell : architecture is "DFF_SR_QQNN";
  attribute pin_number of enable : signal is 14;
  attribute max_wire_delay of clk : signal is 50 ps;
  attribute encoding of idle_state : literal is b"0000";
  attribute cell_position of the_fpu : label is ( 540 um, 1200 um );
  attribute built_in of
    increment [ bit_vector return bit_vector ] : function is bv_incr;
                                                             attribute built_in of
                                                               increment [ std_logic_vector return std_logic_vector ] : function is std_incr;
                                                                                                                                    attribute representation of high [ return speed_range ] : literal is "byte";
                                                                                                                                                                                                         attribute representation of high [ return coolant_level ] : literal is "word";

                                                                                                                                                                                                         begin

                                                                                                                                                                                                           the_fpu : block is
                                                                                                                                                                                                           begin
                                                                                                                                                                                                           end block the_fpu;

                                                                                                                                                                                                           process is
                                                                                                                                                                                                                     variable v1 : string(1 to 11);
                                                                                                                                                                                                                   variable v2 : positive;
                                                                                                                                                                                                                   variable v3 : time;
                                                                                                                                                                                                                   variable v4 : bit_vector(0 to 3);
                                                                                                                                                                                                                   variable v5 : coordinate;
                                                                                                                                                                                                                   variable v6, v7 : built_in_type;
                                                                                                                                                                                                                   variable v8, v9 : string(1 to 4);
                                                                                                                                                                                                           begin

                                                                                                                                                                                                             -- code from book included...

                                                                                                                                                                                                             v1 := std_cell'cell_name ;
                                                                                                                                                                                                             v2 := enable'pin_number ;
                                                                                                                                                                                                             v3 := clk'max_wire_delay ;
                                                                                                                                                                                                             -- workaround MTI bugs mt037/mt038
                                                                                                                                                                                                             -- v4 := idle_state'encoding ;
                                                                                                                                                                                                             v4 := idle_state[return state_type]'encoding ;
                                                                                                                                                                                                             -- end workaround
                                                                                                                                                                                                             v5 := the_fpu'cell_position ;

                                                                                                                                                                                                             v6 := increment [ bit_vector return bit_vector ] 'built_in ;
                                                                                                                                                                                                             v7 := increment [ std_logic_vector return std_logic_vector ] 'built_in ;

                                                                                                                                                                                                             v8 := high [ return speed_range ] 'representation ;
                                                                                                                                                                                                             v9 := high [ return coolant_level ] 'representation ;

                                                                                                                                                                                                             -- end code from book

                                                                                                                                                                                                             wait;
                                                                                                                                                                                                           end process;

                                                                                                                                                                                                         end architecture std_cell;
