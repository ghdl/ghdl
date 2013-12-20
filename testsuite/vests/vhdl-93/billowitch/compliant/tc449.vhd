
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
-- $Id: tc449.vhd,v 1.2 2001-10-26 16:29:54 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY model IS
  PORT
    (
      F1:  OUT     integer := 3;
      F2:  INOUT    integer := 3;
      F3:  IN    integer
      );
END model;

architecture model of model is
begin
  process
  begin
    wait for 1 ns;
    assert F3= 3
      report"wrong initialization of F3 through type conversion" severity failure;
    assert F2 = 3
      report"wrong initialization of F2 through type conversion" severity failure;
    wait;
  end process;
end;


ENTITY c03s02b01x01p19n01i00449ent IS
END c03s02b01x01p19n01i00449ent;

ARCHITECTURE c03s02b01x01p19n01i00449arch OF c03s02b01x01p19n01i00449ent IS

  type boolean_vector    is array (natural range <>) of boolean;
  type severity_level_vector is array (natural range <>) of severity_level;
  type integer_vector    is array (natural range <>) of integer;
  type real_vector    is array (natural range <>) of real;
  type time_vector    is array (natural range <>) of time;
  type natural_vector    is array (natural range <>) of natural;
  type positive_vector    is array (natural range <>) of positive;

  subtype boolean_vector_st    is boolean_vector(0 to 15);
  subtype severity_level_vector_st is  severity_level_vector(0 to 15);
  subtype integer_vector_st    is  integer_vector(0 to 15);
  subtype real_vector_st       is  real_vector(0 to 15);
  subtype time_vector_st       is  time_vector(0 to 15);
  subtype natural_vector_st    is  natural_vector(0 to 15);
  subtype positive_vector_st    is  positive_vector(0 to 15);

  type record_array_st is record
                            a:boolean_vector_st;
                            b:severity_level_vector_st;
                            c:integer_vector_st;
                            d:real_vector_st;
                            e:time_vector_st;
                            f:natural_vector_st;
                            g:positive_vector_st;
                          end record;

  constant C1 : boolean    := true;
  constant C2 : bit    := '1';
  constant C3 : character := 's';
  constant C4 : severity_level    := note;
  constant C5 : integer    := 3;
  constant C6 : real    := 3.0;
  constant C7 : time    := 3 ns;
  constant C8 : natural    := 1;
  constant C9 : positive    := 1;

  constant C70 : boolean_vector_st    :=(others => C1);
  constant C71 : severity_level_vector_st   :=(others => C4);
  constant C72 : integer_vector_st   :=(others => C5);
  constant C73 : real_vector_st      :=(others => C6);
  constant C74 : time_vector_st      :=(others => C7);
  constant C75 : natural_vector_st   :=(others => C8);
  constant C76 : positive_vector_st   :=(others => C9);

  constant C77 : record_array_st := (C70,C71,C72,C73,C74,C75,C76);

  function complex_scalar(s : record_array_st) return integer is
  begin
    return 3;
  end complex_scalar;
  function scalar_complex(s : integer) return record_array_st is
  begin
    return C77;
  end scalar_complex;
  component model1
    PORT
      (    
        F1:  OUT     integer;
        F2:  INOUT    integer;
        F3:  IN    integer
        );
  end component;
  for T1 : model1 use entity work.model(model);

  signal S1 : record_array_st;
  signal S2 : record_array_st;
  signal S3 : record_array_st := C77;
BEGIN
  T1: model1
    port map (
      scalar_complex(F1) => S1,
      scalar_complex(F2) => complex_scalar(S2),
      F3 => complex_scalar(S3)
      );
  TESTING: PROCESS
  BEGIN
    wait for 1 ns;
    assert NOT((S1 = C77) and (S2 = C77)) 
      report "***PASSED TEST: c03s02b01x01p19n01i00449" 
      severity NOTE;
    assert ((S1 = C77) and (S2 = C77)) 
      report "***FAILED TEST: c03s02b01x01p19n01i00449 - For an interface object of mode out, buffer, inout, or linkage, if the formal part includes a type conversion function, then the parameter subtype of that function must be a constrained array subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p19n01i00449arch;
