
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
-- $Id: tc441.vhd,v 1.2 2001-10-26 16:29:54 paw Exp $
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


ENTITY c03s02b01x01p19n01i00441ent IS
END c03s02b01x01p19n01i00441ent;

ARCHITECTURE c03s02b01x01p19n01i00441arch OF c03s02b01x01p19n01i00441ent IS

  type boolean_cons_vector    is array (15 downto 0) of boolean;
  type severity_level_cons_vector is array (15 downto 0) of severity_level;
  type integer_cons_vector    is array (15 downto 0) of integer;
  type real_cons_vector       is array (15 downto 0) of real;
  type time_cons_vector       is array (15 downto 0) of time;
  type natural_cons_vector    is array (15 downto 0) of natural;
  type positive_cons_vector    is array (15 downto 0) of positive;

  type boolean_cons_vectorofvector    is array (0 to 15) of boolean_cons_vector;
  type severity_level_cons_vectorofvector is array (0 to 15) of severity_level_cons_vector;
  type integer_cons_vectorofvector    is array (0 to 15) of integer_cons_vector ;
  type real_cons_vectorofvector       is array (0 to 15) of real_cons_vector;
  type time_cons_vectorofvector       is array (0 to 15) of time_cons_vector;
  type natural_cons_vectorofvector    is array (0 to 15) of natural_cons_vector;
  type positive_cons_vectorofvector    is array (0 to 15) of positive_cons_vector;

  type record_cons_arrayofarray is record
                                     a:boolean_cons_vectorofvector;
                                     b:severity_level_cons_vectorofvector;
                                     c:integer_cons_vectorofvector;
                                     d:real_cons_vectorofvector;
                                     e:time_cons_vectorofvector;
                                     f:natural_cons_vectorofvector;
                                     g:positive_cons_vectorofvector;
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

  constant C26 : boolean_cons_vectorofvector    := (others => (others => C1));
  constant C27 : severity_level_cons_vectorofvector :=  (others => (others => C4));
  constant C28 : integer_cons_vectorofvector    := (others => (others => C5));
  constant C29 : real_cons_vectorofvector    := (others => (others => C6));
  constant C30 : time_cons_vectorofvector    := (others => (others => C7));
  constant C31 : natural_cons_vectorofvector    := (others => (others => C8));
  constant C32 : positive_cons_vectorofvector    := (others => (others => C9));

  constant C53 : record_cons_arrayofarray := (C26,C27,C28,C29,C30,C31,C32);

  function complex_scalar(s : record_cons_arrayofarray) return integer is
  begin
    return 3;
  end complex_scalar;
  function scalar_complex(s : integer) return record_cons_arrayofarray is
  begin
    return C53;
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

  signal S1 : record_cons_arrayofarray;
  signal S2 : record_cons_arrayofarray;
  signal S3 : record_cons_arrayofarray := C53;
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
    assert NOT((S1 = C53) and (S2 = C53)) 
      report "***PASSED TEST: c03s02b01x01p19n01i00441" 
      severity NOTE;
    assert ((S1 = C53) and (S2 = C53)) 
      report "***FAILED TEST: c03s02b01x01p19n01i00441 - For an interface object of mode out, buffer, inout, or linkage, if the formal part includes a type conversion function, then the parameter subtype of that function must be a constrained array subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p19n01i00441arch;
