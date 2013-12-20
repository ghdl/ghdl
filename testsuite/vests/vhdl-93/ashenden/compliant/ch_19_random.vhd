
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
-- $Id: ch_19_random.vhd,v 1.2 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package random is

  type distribution_type is (fixed, uniform, exponential);

  subtype probability is real range 0.0 to 1.0;

  type probability_vector is array (positive range <>) of probability;

  type seed_type is record
                      seed1, seed2 : positive;
                    end record seed_type;
  type seed_array is array ( natural range <> ) of seed_type;
  constant sample_seeds : seed_array(0 to 50);

  type random_info_record is record
                               seed : seed_type;
                               distribution : distribution_type;
                               mean : real;
                               lower_bound, upper_bound : real;
                             end record random_info_record;


  procedure init_fixed ( random_info : out random_info_record;
                         mean : in real );

  procedure init_uniform ( random_info : out random_info_record;
                           lower_bound, upper_bound : in real;
                           seed : in seed_type );

  procedure init_exponential ( random_info : out random_info_record;
                               mean : in real;
                               seed : in seed_type );

  procedure generate_random ( random_info : inout random_info_record;
                              random_number : out real );

end package random;
