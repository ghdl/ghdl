
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
-- $Id: tc485.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p19n01i00485ent_a IS
  PORT
    (
      F1:  OUT     integer := 3;
      F2:  INOUT    integer := 3;
      F3:  IN    integer
      );
END c03s02b01x01p19n01i00485ent_a;

architecture c03s02b01x01p19n01i00485ent_a of c03s02b01x01p19n01i00485ent_a is
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


ENTITY c03s02b01x01p19n01i00485ent IS
END c03s02b01x01p19n01i00485ent;

ARCHITECTURE c03s02b01x01p19n01i00485arch OF c03s02b01x01p19n01i00485ent IS

  type column    is range 1 to 2;
  type row    is range 1 to 8;
  type s2boolean_cons_vector    is array (row,column) of boolean;
  type s2bit_cons_vector    is array (row,column) of bit;
  type s2char_cons_vector    is array (row,column) of character;
  type s2severity_level_cons_vector    is array (row,column) of severity_level;
  type s2integer_cons_vector    is array (row,column) of integer;
  type s2real_cons_vector    is array (row,column) of real;
  type s2time_cons_vector    is array (row,column) of time;
  type s2natural_cons_vector    is array (row,column) of natural;
  type s2positive_cons_vector    is array (row,column) of positive;

  type record_2cons_array is record
                               a:s2boolean_cons_vector;
                               b:s2bit_cons_vector;
                               c:s2char_cons_vector;
                               d:s2severity_level_cons_vector;
                               e:s2integer_cons_vector;
                               f:s2real_cons_vector;
                               g:s2time_cons_vector;
                               h:s2natural_cons_vector;
                               i:s2positive_cons_vector;
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

  constant C41 : s2boolean_cons_vector    := (others => (others => C1));
  constant C42 : s2bit_cons_vector    := (others => (others => C2));
  constant C43 : s2char_cons_vector    := (others => (others => C3));
  constant C44 : s2severity_level_cons_vector    := (others => (others => C4));
  constant C45 : s2integer_cons_vector    := (others => (others => C5));
  constant C46 : s2real_cons_vector    := (others => (others => C6));
  constant C47 : s2time_cons_vector    := (others => (others => C7));
  constant C48 : s2natural_cons_vector    := (others => (others => C8));
  constant C49 : s2positive_cons_vector    := (others => (others => C9));

  constant C52 : record_2cons_array := (C41,C42,C43,C44,C45,C46,C47,C48,C49);

  type    array_rec_2cons is array (integer range <>) of record_2cons_array;

  function resolution12(i:in array_rec_2cons) return record_2cons_array is
    variable temp : record_2cons_array := C52;
  begin
    return temp;
  end resolution12;
  subtype array_rec_2cons_state is resolution12 record_2cons_array;

  constant C66 : array_rec_2cons_state:= C52; 

  function complex_scalar(s : array_rec_2cons_state) return integer is
  begin
    return 3;
  end complex_scalar;
  function scalar_complex(s : integer) return array_rec_2cons_state is
  begin
    return C66;
  end scalar_complex;
  component c03s02b01x01p19n01i00485ent_a1
    PORT
      (    
        F1:  OUT     integer;
        F2:  INOUT    integer;
        F3:  IN    integer
        );
  end component;
  for T1 : c03s02b01x01p19n01i00485ent_a1 use entity work.c03s02b01x01p19n01i00485ent_a(c03s02b01x01p19n01i00485ent_a);

  signal S1 : array_rec_2cons_state;
  signal S2 : array_rec_2cons_state;
  signal S3 : array_rec_2cons_state:= C66;
BEGIN
  T1: c03s02b01x01p19n01i00485ent_a1
    port map (
      scalar_complex(F1) => S1,
      scalar_complex(F2) => complex_scalar(S2),
      F3 => complex_scalar(S3)
      );
  TESTING: PROCESS
  BEGIN
    wait for 1 ns;
    assert NOT((S1 = C66) and (S2 = C66)) 
      report "***PASSED TEST: c03s02b01x01p19n01i00485" 
      severity NOTE;
    assert ((S1 = C66) and (S2 = C66)) 
      report "***FAILED TEST: c03s02b01x01p19n01i00485 - For an interface object of mode out, buffer, inout, or linkage, if the formal part includes a type conversion function, then the parameter subtype of that function must be a constrained array subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p19n01i00485arch;
