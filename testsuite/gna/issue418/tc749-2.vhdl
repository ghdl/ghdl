
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
-- $Id: tc749.vhd,v 1.2 2001-10-26 16:29:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY tc749 IS
  generic(
    zero : integer := 0;
    seven: integer := 7;
    fifteen:integer:= 15;
    C1 : boolean    := true;
    C2 : bit       := '1';
    C3 : character    := 's';
    C4 : severity_level:= note;
    C5 : integer    := 3;
    C6 : real       := 3.0;
    C7 : time       := 3 ns;
    C8 : natural    := 1;
    C9 : positive    := 1
    );
END tc749;

ARCHITECTURE arch OF tc749 IS
  type boolean_cons_vector    is array (fifteen downto zero) of boolean;
  type severity_level_cons_vector    is array (fifteen downto zero) of severity_level;
  type integer_cons_vector    is array (fifteen downto zero) of integer;
  type real_cons_vector    is array (fifteen downto zero) of real;
  type time_cons_vector    is array (fifteen downto zero) of time;
  type natural_cons_vector    is array (fifteen downto zero) of natural;
  type positive_cons_vector    is array (fifteen downto zero) of positive;
  
  type record_cons_array is record
                              a:boolean_cons_vector;
                              b:severity_level_cons_vector;
                              c:integer_cons_vector;
                              d:real_cons_vector;
                              e:time_cons_vector;
                              f:natural_cons_vector;
                              g:positive_cons_vector;
                            end record;
  type array_rec_cons    is array (integer range <>) of record_cons_array;
  
  constant C19 : boolean_cons_vector    := (others => C1);
  constant C20 : severity_level_cons_vector    := (others => C4);
  constant C21 : integer_cons_vector    := (others => C5);
  constant C22 : real_cons_vector    := (others => C6);
  constant C23 : time_cons_vector    :=  (others => C7);
  constant C24 : natural_cons_vector    :=  (others => C8);
  constant C25 : positive_cons_vector    :=  (others => C9);
  constant C51 : record_cons_array    := (C19,C20,C21,C22,C23,C24,C25);
  constant C86: array_rec_cons (0 to 7)    :=(others => C51);

  signal V49   : array_rec_cons(zero to seven) ;


BEGIN
  V49    <= C86;


  TESTING: PROCESS
  BEGIN

    wait for 1 ns;

    assert V49= C86    report " error in initializing S49" severity error;

    wait;
  END PROCESS TESTING;

END arch;
