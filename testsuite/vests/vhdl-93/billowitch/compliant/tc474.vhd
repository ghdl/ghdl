
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
-- $Id: tc474.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
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


ENTITY c03s02b01x01p19n01i00474ent IS
END c03s02b01x01p19n01i00474ent;

ARCHITECTURE c03s02b01x01p19n01i00474arch OF c03s02b01x01p19n01i00474ent IS

  type boolean_vector    is array (natural range <>) of boolean;
  type severity_level_vector    is array (natural range <>) of severity_level;
  type integer_vector    is array (natural range <>) of integer;
  type real_vector    is array (natural range <>) of real;
  type time_vector    is array (natural range <>) of time;
  type natural_vector    is array (natural range <>) of natural;
  type positive_vector    is array (natural range <>) of positive;

  type boolean_cons_vector    is array (15 downto 0) of boolean;
  type severity_level_cons_vector is array (15 downto 0) of severity_level;
  type integer_cons_vector    is array (15 downto 0) of integer;
  type real_cons_vector       is array (15 downto 0) of real;
  type time_cons_vector       is array (15 downto 0) of time;
  type natural_cons_vector    is array (15 downto 0) of natural;
  type positive_cons_vector    is array (15 downto 0) of positive;

  type column    is range 1 to 2;
  type row    is range 1 to 8;
  type s2boolean_cons_vector    is array (row,column) of boolean;
  type s2bit_cons_vector       is array (row,column) of bit;
  type s2char_cons_vector    is array (row,column) of character;
  type s2severity_level_cons_vector is array (row,column) of severity_level;
  type s2integer_cons_vector    is array (row,column) of integer;
  type s2real_cons_vector    is array (row,column) of real;
  type s2time_cons_vector    is array (row,column) of time;
  type s2natural_cons_vector    is array (row,column) of natural;
  type s2positive_cons_vector    is array (row,column) of positive;

  type s2boolean_vector    is array (natural range <>,natural range <>) of boolean;
  type s2bit_vector    is array (natural range<>,natural range <>) of bit;
  type s2char_vector    is array (natural range<>,natural range <>) of character;
  type s2severity_level_vector    is array (natural range <>,natural range <>) of severity_level;
  type s2integer_vector    is array (natural range <>,natural range <>) of integer;
  type s2real_vector    is array (natural range <>,natural range <>) of real;
  type s2time_vector    is array (natural range <>,natural range <>) of time;
  type s2natural_vector    is array (natural range <>,natural range <>) of natural;
  type s2positive_vector    is array (natural range <>,natural range <>) of positive;

  type boolean_cons_vectorofvector    is array (0 to 15) of boolean_cons_vector;
  type severity_level_cons_vectorofvector is array (0 to 15) of severity_level_cons_vector;
  type integer_cons_vectorofvector    is array (0 to 15) of integer_cons_vector ;
  type real_cons_vectorofvector       is array (0 to 15) of real_cons_vector;
  type time_cons_vectorofvector       is array (0 to 15) of time_cons_vector;
  type natural_cons_vectorofvector    is array (0 to 15) of natural_cons_vector;
  type positive_cons_vectorofvector    is array (0 to 15) of positive_cons_vector;

  subtype boolean_vector_st       is  boolean_vector(0 to 15);
  subtype severity_level_vector_st    is  severity_level_vector(0 to 15);
  subtype integer_vector_st       is  integer_vector(0 to 15);
  subtype real_vector_st          is  real_vector(0 to 15);
  subtype time_vector_st          is  time_vector(0 to 15);
  subtype natural_vector_st       is  natural_vector(0 to 15);
  subtype positive_vector_st       is  positive_vector(0 to 15);

  type record_std_package is record
                               a:boolean;
                               b:bit;
                               c:character;
                               d:severity_level;
                               e:integer;
                               f:real;
                               g:time;
                               h:natural;
                               i:positive;
                             end record;

  type record_cons_array is record
                              a:boolean_cons_vector;
                              b:severity_level_cons_vector;
                              c:integer_cons_vector;
                              d:real_cons_vector;
                              e:time_cons_vector;
                              f:natural_cons_vector;
                              g:positive_cons_vector;
                            end record;

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

  type record_cons_arrayofarray is record
                                     a:boolean_cons_vectorofvector;
                                     b:severity_level_cons_vectorofvector;
                                     c:integer_cons_vectorofvector;
                                     d:real_cons_vectorofvector;
                                     e:time_cons_vectorofvector;
                                     f:natural_cons_vectorofvector;
                                     g:positive_cons_vectorofvector;
                                   end record;

  type record_array_st is record
                            a:boolean_vector_st;
                            b:severity_level_vector_st;
                            c:integer_vector_st;
                            d:real_vector_st;
                            e:time_vector_st;
                            f:natural_vector_st;
                            g:positive_vector_st;
                          end record;

  type record_of_records is record
                              a: record_std_package;
                              c: record_cons_array;
                              e: record_2cons_array;
                              g: record_cons_arrayofarray;
                              i: record_array_st;
                            end record;

  type array_rec_rec is array (integer range <>) of record_of_records;
  
  constant C1 : boolean    := true;
  constant C2 : bit    := '1';
  constant C3 : character := 's';
  constant C4 : severity_level    := note;
  constant C5 : integer    := 3;
  constant C6 : real    := 3.0;
  constant C7 : time    := 3 ns;
  constant C8 : natural    := 1;
  constant C9 : positive    := 1;

  constant C19 : boolean_cons_vector       := (others => C1);
  constant C20 : severity_level_cons_vector    := (others => C4);
  constant C21 : integer_cons_vector       := (others => C5);
  constant C22 : real_cons_vector       := (others => C6);
  constant C23 : time_cons_vector       := (others => C7);
  constant C24 : natural_cons_vector       := (others => C8);
  constant C25 : positive_cons_vector       := (others => C9);
  constant C26 : boolean_cons_vectorofvector    := (others => (others => C1));
  constant C27 : severity_level_cons_vectorofvector    :=  (others => (others => C4));
  constant C28 : integer_cons_vectorofvector    := (others => (others => C5));
  constant C29 : real_cons_vectorofvector    := (others => (others => C6));
  constant C30 : time_cons_vectorofvector    := (others => (others => C7));
  constant C31 : natural_cons_vectorofvector    := (others => (others => C8));
  constant C32 : positive_cons_vectorofvector    := (others => (others => C9));
  constant C41 : s2boolean_cons_vector    := (others => (others => C1));
  constant C42 : s2bit_cons_vector    := (others => (others => C2));
  constant C43 : s2char_cons_vector    := (others => (others => C3));
  constant C44 : s2severity_level_cons_vector    := (others => (others => C4));
  constant C45 : s2integer_cons_vector    := (others => (others => C5));
  constant C46 : s2real_cons_vector    := (others => (others => C6));
  constant C47 : s2time_cons_vector    := (others => (others => C7));
  constant C48 : s2natural_cons_vector    := (others => (others => C8));
  constant C49 : s2positive_cons_vector    := (others => (others => C9));
  constant C50 : record_std_package    := (C1,C2,C3,C4,C5,C6,C7,C8,C9);
  constant C51 : record_cons_array    := (C19,C20,C21,C22,C23,C24,C25);
  constant C52 : record_2cons_array    := (C41,C42,C43,C44,C45,C46,C47,C48,C49);
  constant C53 : record_cons_arrayofarray := (C26,C27,C28,C29,C30,C31,C32);
  constant C70 : boolean_vector_st    := (others => C1);
  constant C71 : severity_level_vector_st   := (others => C4);
  constant C72 : integer_vector_st   := (others => C5);
  constant C73 : real_vector_st      := (others => C6);
  constant C74 : time_vector_st      := (others => C7);
  constant C75 : natural_vector_st   := (others => C8);
  constant C76 : positive_vector_st   := (others => C9);

  constant C77 : record_array_st       := (C70,C71,C72,C73,C74,C75,C76);

  constant C55 : record_of_records    := (C50,C51,C52,C53,C77);

  constant C66 : array_rec_rec(0 to 7)    := (others => C55);

  function complex_scalar(s : array_rec_rec(0 to 7)) return integer is
  begin
    return 3;
  end complex_scalar;
  function scalar_complex(s : integer) return array_rec_rec is
  begin
    return C66;
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

  signal S1 : array_rec_rec(0 to 7);
  signal S2 : array_rec_rec(0 to 7);
  signal S3 : array_rec_rec(0 to 7):= C66;
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
    assert NOT((S1 = C66) and (S2 = C66)) 
      report "***PASSED TEST: c03s02b01x01p19n01i00474" 
      severity NOTE;
    assert ((S1 = C66) and (S2 = C66)) 
      report "***FAILED TEST: c03s02b01x01p19n01i00474 - For an interface object of mode out, buffer, inout, or linkage, if the formal part includes a type conversion function, then the parameter subtype of that function must be a constrained array subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p19n01i00474arch;
