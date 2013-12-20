
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
-- $Id: ch_19_random-b.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library math;

package body random is

  use math.math_real;

  constant sample_seeds : seed_array(0 to 50)
    := (  0 => (1, 1),
          1 => (1919456777, 2006618587),
          2 => (928906921, 476680813),
          3 => (715788085, 762347824),
          4 => (366002668, 1804336679),
          5 => (1866585254, 247488051),
          6 => (1342990589, 1539624735),
          7 => (677313287, 1675609237),
          8 => (644816519, 2026475269),
          9 => (1654953611, 564421524),
          10 => (1020104619, 712556314),
          11 => (609798541, 1592526288),
          12 => (1106087470, 1468242308),
          13 => (1378844312, 646793513),
          14 => (966261604, 481733031),
          15 => (1407842093, 1316990206),
          16 => (1705378215, 1930221363),
          17 => (206887499, 1810320799),
          18 => (1681633030, 2114795480),
          19 => (71194926, 1642522201),
          20 => (663275331, 1947299255),
          21 => (224432387, 944962866),
          22 => (1156075861, 1866435087),
          23 => (1670357576, 1247152991),
          24 => (846934138, 1673364736),
          25 => (1972636955, 1404522710),
          26 => (533484185, 592078395),
          27 => (1989468008, 1409246301),
          28 => (697086615, 1975145057),
          29 => (111393259, 1673620688),
          30 => (1352201163, 872947497),
          31 => (1342844190, 877696585),
          32 => (938770066, 1222894811),
          33 => (1144599578, 661919919),
          34 => (1750521407, 269946538),
          35 => (457892500, 1256953520),
          36 => (1678589945, 356027520),
          37 => (1484458924, 2103068828),
          38 => (1296978761, 2124096638),
          39 => (1702642440, 1161000593),
          40 => (1244690090, 2016422304),
          41 => (1858682943, 1053836731),
          42 => (1496964676, 701079294),
          43 => (432696952, 602526767),
          44 => (2097684438, 1264032473),
          45 => (2115456834, 298917738),
          46 => (432301768, 232430346),
          47 => (1929812456, 758157910),
          48 => (1655564027, 1062345086),
          49 => (1116121051, 538424126),
          50 => (844396720, 821616997) );


  procedure init_fixed ( random_info : out random_info_record;
                         mean : in real ) is
  begin
    random_info.distribution := fixed;
    random_info.mean := mean;
  end procedure init_fixed;


  procedure init_uniform ( random_info : out random_info_record;
                           lower_bound, upper_bound : in real;
                           seed : in seed_type ) is
  begin
    assert lower_bound <= upper_bound
                          report "init_uniform: lower_bound > upper_bound" severity failure;
    random_info.distribution := uniform;
    random_info.lower_bound := lower_bound;
    random_info.upper_bound := upper_bound;
    random_info.seed := seed;
  end procedure init_uniform;


  procedure init_exponential ( random_info : out random_info_record;
                               mean : in real;
                               seed : in seed_type ) is
  begin
    assert mean > 0.0
      report "init_exponential: mean not positive" severity failure;
    random_info.distribution := exponential;
    random_info.mean := mean;
    random_info.seed := seed;
  end procedure init_exponential;


  procedure generate_uniform ( random_info : inout random_info_record;
                               random_number : out real ) is
    variable tmp : real;
  begin
    math_real.uniform(random_info.seed.seed1, random_info.seed.seed2, tmp);
    random_number := random_info.lower_bound
                     + tmp * (random_info.upper_bound - random_info.lower_bound);
  end procedure generate_uniform;


  procedure generate_exponential ( random_info : inout random_info_record;
                                   random_number : out real ) is
    variable tmp : real;
  begin
    loop
      math_real.uniform(random_info.seed.seed1, random_info.seed.seed2, tmp);
      exit when tmp /= 0.0;
    end loop;
    random_number := - random_info.mean * math_real.log(tmp);
  end procedure generate_exponential;


  procedure generate_random ( random_info : inout random_info_record;
                              random_number : out real ) is
  begin
    case random_info.distribution is
      when fixed =>
        random_number := random_info.mean;
      when uniform =>
        generate_uniform(random_info, random_number);
      when exponential =>
        generate_exponential(random_info, random_number);
    end case;
  end procedure generate_random;

end package body random;
