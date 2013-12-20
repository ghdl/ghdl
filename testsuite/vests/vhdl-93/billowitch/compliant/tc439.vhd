
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
-- $Id: tc439.vhd,v 1.2 2001-10-26 16:29:54 paw Exp $
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


ENTITY c03s02b01x01p19n01i00439ent IS
END c03s02b01x01p19n01i00439ent;

ARCHITECTURE c03s02b01x01p19n01i00439arch OF c03s02b01x01p19n01i00439ent IS

  type boolean_cons_vector    is array (15 downto 0) of boolean;
  type severity_level_cons_vector is array (15 downto 0) of severity_level;
  type integer_cons_vector    is array (15 downto 0) of integer;
  type real_cons_vector       is array (15 downto 0) of real;
  type time_cons_vector       is array (15 downto 0) of time;
  type natural_cons_vector    is array (15 downto 0) of natural;
  type positive_cons_vector    is array (15 downto 0) of positive;

  type record_cons_array is record
                              a:boolean_cons_vector;
                              b:severity_level_cons_vector;
                              c:integer_cons_vector;
                              d:real_cons_vector;
                              e:time_cons_vector;
                              f:natural_cons_vector;
                              g:positive_cons_vector;
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

  constant C19 : boolean_cons_vector    := (others => C1);
  constant C20 : severity_level_cons_vector := (others => C4);
  constant C21 : integer_cons_vector    := (others => C5);
  constant C22 : real_cons_vector    := (others => C6);
  constant C23 : time_cons_vector    := (others => C7);
  constant C24 : natural_cons_vector    := (others => C8);
  constant C25 : positive_cons_vector    := (others => C9);

  constant    C51 : record_cons_array := (C19,C20,C21,C22,C23,C24,C25);

  function complex_scalar(s : record_cons_array) return integer is
  begin
    return 3;
  end complex_scalar;
  function scalar_complex(s : integer) return record_cons_array is
  begin
    return C51;
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

  signal S1 : record_cons_array;
  signal S2 : record_cons_array;
  signal S3 : record_cons_array := C51;
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
    assert NOT((S1 = C51) and (S2 = C51)) 
      report "***PASSED TEST: c03s02b01x01p19n01i00439" 
      severity NOTE;
    assert ((S1 = C51) and (S2 = C51)) 
      report "***FAILED TEST: c03s02b01x01p19n01i00439 - For an interface object of mode out, buffer, inout, or linkage, if the formal part includes a type conversion function, then the parameter subtype of that function must be a constrained array subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p19n01i00439arch;
