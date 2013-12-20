
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
-- $Id: ch_07_fg_07_16.vhd,v 1.2 2001-10-26 16:29:34 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity fg_07_16 is
end entity fg_07_16;



architecture test of fg_07_16 is

  -- code from book

  function limit ( value, min, max : integer ) return integer is
  begin
    if value > max then
      return max;
    elsif value < min then
      return min;
    else
      return value;
    end if;
  end function limit;

  -- end code from book

begin

  tester : process is

                     variable new_temperature, current_temperature, increment : integer;
                   variable new_motor_speed, old_motor_speed,
                     scale_factor, error : integer;

  begin

    current_temperature := 75;
    increment := 10;

    -- code from book (in text)

    new_temperature := limit ( current_temperature + increment, 10, 100 );

    -- end code from book

    increment := 60;
    new_temperature := limit ( current_temperature + increment, 10, 100 );
    increment := -100;
    new_temperature := limit ( current_temperature + increment, 10, 100 );

    old_motor_speed := 1000;
    scale_factor := 5;
    error := 5;

    -- code from book (in text)

    new_motor_speed := old_motor_speed
                       + scale_factor * limit ( error, -10, +10 );

    -- end code from book

    error := 15;
    new_motor_speed := old_motor_speed
                       + scale_factor * limit ( error, -10, +10 );

    error := -20;
    new_motor_speed := old_motor_speed
                       + scale_factor * limit ( error, -10, +10 );

    wait;
  end process tester;

end architecture test;
