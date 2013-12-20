
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

library ieee;  use ieee.std_logic_1164.all;

package stimulus_generators is

  procedure all_possible_values ( signal bv : out bit_vector;
				  delay_between_values : in delay_length );

  procedure all_possible_values ( signal bv : out std_ulogic_vector;
				  delay_between_values : in delay_length );

  procedure all_possible_values ( signal bv : out std_logic_vector;
				  delay_between_values : in delay_length );

end package stimulus_generators;



package body stimulus_generators is

  type digit_table is array ( natural range 0 to 1 ) of bit;
  constant digit : digit_table := ( '0', '1' );


  function natural_to_bv ( nat : in natural;
      	      	      	   length : in natural ) return bit_vector is

    variable temp : natural := nat;
    variable result : bit_vector(0 to length - 1);

  begin
    for index in result'reverse_range loop
      result(index) := digit( temp rem 2 );
      temp := temp / 2;
    end loop;
    return result;
  end function natural_to_bv;


  procedure all_possible_values ( signal bv : out bit_vector;
				  delay_between_values : in delay_length ) is
  begin
    bv <= natural_to_bv(0, bv'length);
    for value in 1 to 2**bv'length - 1 loop
      wait for delay_between_values;
      bv <= natural_to_bv(value, bv'length);
    end loop;
  end procedure all_possible_values;


  procedure all_possible_values ( signal bv : out std_ulogic_vector;
				  delay_between_values : in delay_length ) is
  begin
    bv <= To_StdULogicVector(natural_to_bv(0, bv'length));
    for value in 1 to 2**bv'length - 1 loop
      wait for delay_between_values;
      bv <= To_StdULogicVector(natural_to_bv(value, bv'length));
    end loop;
  end procedure all_possible_values;


  procedure all_possible_values ( signal bv : out std_logic_vector;
				  delay_between_values : in delay_length ) is
  begin
    bv <= To_StdLogicVector(natural_to_bv(0, bv'length));
    for value in 1 to 2**bv'length - 1 loop
      wait for delay_between_values;
      bv <= To_StdLogicVector(natural_to_bv(value, bv'length));
    end loop;
  end procedure all_possible_values;

end package body stimulus_generators;
