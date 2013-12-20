
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

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
-- $Id: tc763.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b01x01p05n02i00763pkg is

--UNCONSTRAINED ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  type boolean_vector    is array (natural range <>) of boolean;
  type severity_level_vector    is array (natural range <>) of severity_level;
  type integer_vector    is array (natural range <>) of integer;
  type real_vector    is array (natural range <>) of real;
  type time_vector    is array (natural range <>) of time;
  type natural_vector    is array (natural range <>) of natural;
  type positive_vector is array (natural range <>) of positive;

--CONSTRAINED ARRAY OF TYPES FROM STANDARD PACKAGE
--Index type is natural
  subtype boolean_vector_st       is  boolean_vector(0 to 15);
  subtype severity_level_vector_st    is  severity_level_vector(0 to 15);
  subtype integer_vector_st       is  integer_vector(0 to 15);
  subtype real_vector_st       is  real_vector(0 to 15);
  subtype time_vector_st       is  time_vector(0 to 15);
  subtype natural_vector_st       is  natural_vector(0 to 15);
  subtype positive_vector_st       is  positive_vector(0 to 15);

  constant C1    : boolean    := true; 
  constant C2    : bit       := '1';
  constant C3    : character    := 's';
  constant C4    : severity_level:= note; 
  constant C5    : integer    := 3;
  constant C6    : real       := 3.0;
  constant C7    : time       := 3 ns;
  constant C8    : natural    := 1;
  constant C9    : positive    := 1;
  constant C70    : boolean_vector_st       :=(others => C1);
  constant C71    : severity_level_vector_st   :=(others => C4);
  constant C72    : integer_vector_st      :=(others => C5);
  constant C73    : real_vector_st      :=(others => C6);
  constant C74    : time_vector_st      :=(others => C7);
  constant C75    : natural_vector_st      :=(others => C8);
  constant C76    : positive_vector_st      :=(others => C9);

end c01s01b01x01p05n02i00763pkg;

use   work.c01s01b01x01p05n02i00763pkg.ALL;
ENTITY c01s01b01x01p05n02i00763ent IS
  generic(
    zero    : integer := 0;
    one     : integer := 1;
    two     : integer := 2;
    three   : integer := 3;
    four    : integer := 4;
    five    : integer := 5;
    six     : integer := 6;
    seven   : integer := 7;
    eight   : integer := 8;
    nine    : integer := 9;
    fifteen   :integer:= 15;
    Cgen1    : boolean := true;
    Cgen2    : bit := '1';
    Cgen3    : character := 's';
    Cgen4    : severity_level := note;
    Cgen5    : integer := 3;
    Cgen6    : real := 3.0;
    Cgen7    : time := 3 ns;
    Cgen8    : natural := 1;
    Cgen9    : positive := 1;
    Cgen70  : boolean_vector_st       :=(others => true);
    Cgen71  : severity_level_vector_st   :=(others => note);
    Cgen72  : integer_vector_st      :=(others => 3);
    Cgen73  : real_vector_st      :=(others => 3.0);
    Cgen74  : time_vector_st      :=(others => 3 ns);
    Cgen75  : natural_vector_st      :=(others => 1);
    Cgen76  : positive_vector_st      :=(others => 1)
    );
END c01s01b01x01p05n02i00763ent;

ARCHITECTURE c01s01b01x01p05n02i00763arch OF c01s01b01x01p05n02i00763ent IS
  signal Vgen1    : boolean    := true;
  signal Vgen2    : bit       := '1';
  signal Vgen3    : character    := 's';
  signal Vgen4    : severity_level:= note;
  signal Vgen5    : integer    := 3;
  signal Vgen6    : real       := 3.0;
  signal Vgen7    : time       := 3 ns;
  signal Vgen8    : natural    := 1;
  signal Vgen9    : positive    := 1;
  signal Vgen70   : boolean_vector_st       :=(others => Cgen1);
  signal Vgen71   : severity_level_vector_st   :=(others => Cgen4);
  signal Vgen72   : integer_vector_st      :=(others => Cgen5);
  signal Vgen73   : real_vector_st      :=(others => Cgen6);
  signal Vgen74   : time_vector_st      :=(others => Cgen7);
  signal Vgen75   : natural_vector_st      :=(others => Cgen8);
  signal Vgen76   : positive_vector_st      :=(others => Cgen9);
  
BEGIN
  assert Vgen1 = C1 report "Initializing signal with generic Vgen1 does not work" severity error;
  assert Vgen2 = C2 report "Initializing signal with generic Vgen2 does not work" severity error;
  assert Vgen3 = C3 report "Initializing signal with generic Vgen3 does not work" severity error;
  assert Vgen4 = C4 report "Initializing signal with generic Vgen4 does not work" severity error;
  assert Vgen5 = C5 report "Initializing signal with generic Vgen5 does not work" severity error;
  assert Vgen6 = C6 report "Initializing signal with generic Vgen6 does not work" severity error;
  assert Vgen7 = C7 report "Initializing signal with generic Vgen7 does not work" severity error;
  assert Vgen8 = C8 report "Initializing signal with generic Vgen8 does not work" severity error;
  assert Vgen9 = C9 report "Initializing signal with generic Vgen9 does not work" severity error;
  assert Vgen70 = C70 report "Initializing signal with generic Vgen70 does not work" severity error;
  assert Vgen71 = C71 report "Initializing signal with generic Vgen71 does not work" severity error;
  assert Vgen72 = C72 report "Initializing signal with generic Vgen72 does not work" severity error;
  assert Vgen73 = C73 report "Initializing signal with generic Vgen73 does not work" severity error;
  assert Vgen74 = C74 report "Initializing signal with generic Vgen74 does not work" severity error;
  assert Vgen75 = C75 report "Initializing signal with generic Vgen75 does not work" severity error;
  assert Vgen76 = C76 report "Initializing signal with generic Vgen76 does not work" severity error;

  TESTING: PROCESS
  BEGIN

    assert NOT(     Vgen1 = C1 and 
                    Vgen2 = C2 and 
                    Vgen3 = C3 and 
                    Vgen4 = C4 and 
                    Vgen5 = C5 and 
                    Vgen6 = C6 and 
                    Vgen7 = C7 and 
                    Vgen8 = C8 and 
                    Vgen9 = C9 and 
                    Vgen70 = C70 and
                    Vgen71 = C71 and
                    Vgen72 = C72 and
                    Vgen73 = C73 and
                    Vgen74 = C74 and
                    Vgen75 = C75 and
                    Vgen76 = C76 )
      report "***PASSED TEST: c01s01b01x01p05n02i00763"
      severity NOTE;
    assert(        Vgen1 = C1 and 
                   Vgen2 = C2 and 
                   Vgen3 = C3 and 
                   Vgen4 = C4 and 
                   Vgen5 = C5 and 
                   Vgen6 = C6 and 
                   Vgen7 = C7 and 
                   Vgen8 = C8 and 
                   Vgen9 = C9 and 
                   Vgen70 = C70 and
                   Vgen71 = C71 and
                   Vgen72 = C72 and
                   Vgen73 = C73 and
                   Vgen74 = C74 and
                   Vgen75 = C75 and
                   Vgen76 = C76 )
      report "***FAILED TEST: c01s01b01x01p05n02i00763 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00763arch;
