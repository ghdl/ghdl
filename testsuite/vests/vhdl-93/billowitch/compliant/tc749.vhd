
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

ENTITY c01s01b01x01p05n02i00749ent IS
  generic(
    zero : integer := 0;
    one  : integer := 1;
    two  : integer := 2;
    three: integer := 3;
    four : integer := 4;
    five : integer := 5;
    six  : integer := 6;
    seven: integer := 7;
    eight: integer := 8;
    nine : integer := 9;
    fifteen:integer:= 15;
    C1 : boolean    := true;
    C2 : bit       := '1';
    C3 : character    := 's';
    C4 : severity_level:= note;
    C5 : integer    := 3;
    C6 : real       := 3.0;
    C7 : time       := 3 ns;
    C8 : natural    := 1;
    C9 : positive    := 1;
    C10 : string    := "shishir";
    C11 : bit_vector    := B"0011"
    );
END c01s01b01x01p05n02i00749ent;

ARCHITECTURE c01s01b01x01p05n02i00749arch OF c01s01b01x01p05n02i00749ent IS
  subtype hi_to_low_range    is integer range zero to seven;
  type boolean_vector       is array (natural range <>) of boolean;
  type severity_level_vector    is array (natural range <>) of severity_level;
  type integer_vector       is array (natural range <>) of integer;
  type real_vector       is array (natural range <>) of real;
  type time_vector       is array (natural range <>) of time;
  type natural_vector       is array (natural range <>) of natural;
  type positive_vector    is array (natural range <>) of positive;

  subtype boolean_vector_st    is boolean_vector(zero to fifteen);
  subtype severity_level_vector_st    is  severity_level_vector(zero to fifteen);
  subtype integer_vector_st    is  integer_vector(zero to fifteen);
  subtype real_vector_st    is  real_vector(zero to fifteen);
  subtype time_vector_st    is  time_vector(zero to fifteen);
  subtype natural_vector_st    is  natural_vector(zero to fifteen);
  subtype positive_vector_st    is  positive_vector(zero to fifteen);

  type boolean_cons_vector    is array (fifteen downto zero) of boolean;
  type severity_level_cons_vector    is array (fifteen downto zero) of severity_level;
  type integer_cons_vector    is array (fifteen downto zero) of integer;
  type real_cons_vector    is array (fifteen downto zero) of real;
  type time_cons_vector    is array (fifteen downto zero) of time;
  type natural_cons_vector    is array (fifteen downto zero) of natural;
  type positive_cons_vector    is array (fifteen downto zero) of positive;
  
  type boolean_cons_vectorofvector    is array (zero to fifteen) of boolean_cons_vector;   
  type severity_level_cons_vectorofvector    is array (zero to fifteen) of severity_level_cons_vector;
  type integer_cons_vectorofvector    is array (zero to fifteen) of integer_cons_vector
    ;
  type real_cons_vectorofvector    is array (zero to fifteen) of real_cons_vector;
  type time_cons_vectorofvector    is array (zero to fifteen) of time_cons_vector;
  type natural_cons_vectorofvector    is array (zero to fifteen) of natural_cons_vector;   
  type positive_cons_vectorofvector    is array (zero to fifteen) of positive_cons_vector;
  
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
                               j:string(one to seven);
                               k:bit_vector(zero to three);
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
  
  type record_cons_array is record
                              a:boolean_cons_vector;
                              b:severity_level_cons_vector;
                              c:integer_cons_vector;
                              d:real_cons_vector;
                              e:time_cons_vector;
                              f:natural_cons_vector;
                              g:positive_cons_vector;
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
  
  type record_array_new is record
                             a:boolean_vector(zero to fifteen);
                             b:severity_level_vector(zero to fifteen);
                             c:integer_vector(zero to fifteen);
                             d:real_vector(zero to fifteen);
                             e:time_vector(zero to fifteen);
                             f:natural_vector(zero to fifteen);
                             g:positive_vector(zero to fifteen);
                           end record;
  
  type record_of_records is record
                              a: record_std_package;
                              c: record_cons_array;
                              g: record_cons_arrayofarray;
                              i: record_array_st;
                              j: record_array_new;
                            end record;
  
  subtype boolean_vector_range    is boolean_vector(hi_to_low_range);
  subtype severity_level_vector_range    is severity_level_vector(hi_to_low_range);
  subtype integer_vector_range    is integer_vector(hi_to_low_range);
  subtype real_vector_range       is real_vector(hi_to_low_range);
  subtype time_vector_range       is time_vector(hi_to_low_range);
  subtype natural_vector_range    is natural_vector(hi_to_low_range);
  subtype positive_vector_range    is positive_vector(hi_to_low_range);

  type array_rec_std    is array (integer range <>) of record_std_package;
  type array_rec_cons    is array (integer range <>) of record_cons_array;
  type array_rec_rec    is array (integer range <>) of record_of_records;
  
  subtype array_rec_std_st    is array_rec_std (hi_to_low_range);
  subtype array_rec_cons_st    is array_rec_cons (hi_to_low_range);
  subtype array_rec_rec_st    is array_rec_rec (hi_to_low_range);
  
  type record_of_arr_of_record is record
                                    a: array_rec_std(zero to seven);
                                    b: array_rec_cons(zero to seven);
                                    c: array_rec_rec(zero to seven);
                                  end record;
  
  type current is range -2147483647 to +2147483647
    units
      nA;
      uA = 1000 nA;
      mA = 1000 uA;
      A  = 1000 mA;
    end units;
  
  type current_vector is array (natural range <>) of current;
  
  subtype current_vector_range is current_vector(hi_to_low_range);
  
  type resistance is range -2147483647 to +2147483647
    units
      uOhm;
      mOhm = 1000 uOhm;
      Ohm = 1000 mOhm;
      KOhm  = 1000 Ohm;
    end units;
  
  type resistance_vector is array (natural range <>) of resistance;
  
  subtype resistance_vector_range is resistance_vector(hi_to_low_range);
  
  type byte is array(zero to seven) of bit;
  
  subtype word is bit_vector(zero to fifteen);                            --constrained array
  
  constant size :integer := seven;
  
  type primary_memory is array(zero to size) of word;                     --array of an array
  type primary_memory_module is                                           --record with field
    record                                                                  --as an array
      enable:bit;
      memory_number:primary_memory;
    end record;
  type whole_memory is array(0 to size) of primary_memory_module;         --array of a complex record
  subtype delay is integer range one to 10;
  
  constant C12 : boolean_vector    := (C1,false);
  constant C13 : severity_level_vector    := (C4,error);
  constant C14 : integer_vector    := (one,two,three,four);
  constant C15 : real_vector    := (1.0,2.0,C6,4.0);
  constant C16 : time_vector    := (1 ns, 2 ns,C7, 4 ns);
  constant C17 : natural_vector    := (one,2,3,4);
  constant C18 : positive_vector    := (one,2,3,4);
  constant C19 : boolean_cons_vector    := (others => C1);
  constant C20 : severity_level_cons_vector    := (others => C4);
  constant C21 : integer_cons_vector    := (others => C5);
  constant C22 : real_cons_vector    := (others => C6);
  constant C23 : time_cons_vector    :=  (others => C7);
  constant C24 : natural_cons_vector    :=  (others => C8);
  constant C25 : positive_cons_vector    :=  (others => C9);
  constant C26 : boolean_cons_vectorofvector    := (others => (others => C1));
  constant C27 : severity_level_cons_vectorofvector    :=  (others => (others => C4));
  constant C28 : integer_cons_vectorofvector    := (others => (others => C5));
  constant C29 : real_cons_vectorofvector    := (others => (others => C6));
  constant C30 : time_cons_vectorofvector    := (others => (others => C7));
  constant C31 : natural_cons_vectorofvector    := (others => (others => C8));
  constant C32 : positive_cons_vectorofvector    := (others => (others => C9));
  constant C50 : record_std_package    := (C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11);
  constant C51 : record_cons_array    := (C19,C20,C21,C22,C23,C24,C25);
  constant C53 : record_cons_arrayofarray := (C26,C27,C28,C29,C30,C31,C32);
  constant C70 : boolean_vector_st :=(others => C1);
  constant C71 : severity_level_vector_st   := (others => C4);
  constant C72 : integer_vector_st:=(others => C5);
  constant C73 : real_vector_st   :=(others => C6);
  constant C74 : time_vector_st   :=(others => C7);
  constant C75 : natural_vector_st:=(others => C8);
  constant C76 : positive_vector_st:=(others => C9);
  constant C77 : record_array_st    := (C70,C71,C72,C73,C74,C75,C76);
  constant C54a :record_array_st    := (C70,C71,C72,C73,C74,C75,C76);
  constant C54b: record_array_new   := (C70,C71,C72,C73,C74,C75,C76);
  constant C55 : record_of_records:= (C50,C51,C53,C77,C54b);
  constant C60 : byte       := (others => '0');
  constant C61 : word       := (others =>'0' );
  constant C64 : primary_memory    := (others => C61);
  constant C65 : primary_memory_module    := ('1',C64);
  constant C66 : whole_memory    := (others => C65);
  constant C67 : current       := 1 A;
  constant C68 : resistance    := 1 Ohm;
  constant C69 : delay       := 2;
  constant C78: boolean_vector_range    := (others => C1);
  constant C79: severity_level_vector_range    := (others => C4) ;
  constant C80: integer_vector_range    :=(others => C5) ;
  constant C81: real_vector_range    :=(others => C6);
  constant C82: time_vector_range    :=(others => C7);
  constant C83: natural_vector_range    :=(others => C8);
  constant C84: positive_vector_range    :=(others => C9);
  constant C85: array_rec_std(0 to 7)    :=(others => C50)  ;
  constant C86: array_rec_cons (0 to 7)    :=(others => C51);
  constant C88: array_rec_rec(0 to 7)    :=(others => C55);
  constant C102: record_of_arr_of_record   := (C85,C86,C88);

  signal V1    : boolean_vector(zero to fifteen) ;
  signal V2    : severity_level_vector(zero to fifteen);
  signal V3    : integer_vector(zero to fifteen) ;
  signal V4    : real_vector(zero to fifteen) ;
  signal V5    : time_vector (zero to fifteen);
  signal V6    : natural_vector(zero to fifteen);
  signal V7    : positive_vector(zero to fifteen);
  signal V8    : boolean_cons_vector;
  signal V9    : severity_level_cons_vector ;
  signal V10    : integer_cons_vector;
  signal V11    : real_cons_vector;
  signal V12    : time_cons_vector ;
  signal V13    : natural_cons_vector ;
  signal V14    : positive_cons_vector ;
  signal V15    : boolean_cons_vectorofvector ;
  signal V16    : severity_level_cons_vectorofvector;
  signal V17    : integer_cons_vectorofvector;
  signal V18    : real_cons_vectorofvector;
  signal V19    : time_cons_vectorofvector;
  signal V20    : natural_cons_vectorofvector;
  signal V21    : positive_cons_vectorofvector;
  signal V22    : record_std_package;
  signal V23    : record_cons_array ;
  signal V24    : record_cons_arrayofarray ;
  signal V25    : boolean_vector_st ;
  signal V26    : severity_level_vector_st ;
  signal V27    : integer_vector_st ;
  signal V28    : real_vector_st ;
  signal V29    : time_vector_st ;
  signal V30    : natural_vector_st ;
  signal V31    : positive_vector_st ;
  signal V32    : record_array_st ;
  signal V33    : record_array_st ;
  signal V34   : record_array_new ;
  signal V35    : record_of_records ;
  signal V36    : byte ;
  signal V37    : word ;
  signal V41   : boolean_vector_range ;
  signal V42   : severity_level_vector_range  ;
  signal V43   : integer_vector_range  ;
  signal V44   : real_vector_range ;
  signal V45   : time_vector_range  ;
  signal V46   : natural_vector_range  ;
  signal V47   : positive_vector_range  ;
  signal V48   : array_rec_std(zero to seven) ;
  signal V49   : array_rec_cons(zero to seven) ;
  signal V50   : array_rec_rec(zero to seven) ;
  signal V51   : record_of_arr_of_record ;

BEGIN
  V1     <= (zero to fifteen => C1);
  V2     <= (zero to fifteen => C4);
  V3     <= (zero to fifteen => C5);
  V4     <= (zero to fifteen => C6);
  V5     <= (zero to fifteen => C7);
  V6     <= (zero to fifteen => C8);
  V7     <= (zero to fifteen => C9);
  V8     <= C19;
  V9     <= C20;
  V10     <= C21;
  V11     <= C22;
  V12     <= C23;
  V13     <= C24;
  V14     <= C25;
  V15     <= C26;
  V16     <= C27;
  V17     <= C28;
  V18     <= C29;
  V19     <= C30;
  V20     <= C31;
  V21     <= C32;
  V22     <= C50;
  V23     <= C51;
  V24     <= C53;
  V25     <= C70;
  V26     <= C71;
  V27     <= C72;
  V28     <= C73;
  V29     <= C74;
  V30     <= C75;
  V31     <= C76;
  V32     <= C54a;
  V33     <= C54a;
  V34    <= C54b;
  V35     <= C55;
  V36     <= C60;
  V37     <= C61;
  V41    <= C78;
  V42    <= C79;
  V43    <= C80;
  V44    <= C81;
  V45    <= C82;
  V46    <= C83;
  V47    <= C84;
  V48    <= C85;
  V49    <= C86;
  V50    <= C88;
  V51    <= C102;

  TESTING: PROCESS
  BEGIN

    wait for 1 ns;

    assert (V1(0) = C1)    report " error in initializing S1" severity error;
    assert (V2(0) = C4)    report " error in initializing S2" severity error;
    assert (V3(0) = C5)    report " error in initializing S3" severity error;
    assert (V4(0) = C6)    report " error in initializing S4" severity error;
    assert (V5(0) = C7)    report " error in initializing S5" severity error;
    assert (V6(0) = C8)    report " error in initializing S6" severity error;
    assert (V7(0) = C9)    report " error in initializing S7" severity error;
    assert V8 = C19    report " error in initializing S8" severity error;
    assert V9 = C20    report " error in initializing S9" severity error;
    assert V10 = C21    report " error in initializing S10" severity error;
    assert V11 = C22    report " error in initializing S11" severity error;
    assert V12 = C23    report " error in initializing S12" severity error;
    assert V13 = C24    report " error in initializing S13" severity error;
    assert V14 = C25    report " error in initializing S14" severity error;
    assert V15 = C26    report " error in initializing S15" severity error;
    assert V16 = C27    report " error in initializing S16" severity error;
    assert V17 = C28    report " error in initializing S17" severity error;
    assert V18 = C29    report " error in initializing S18" severity error;
    assert V19 = C30    report " error in initializing S19" severity error;
    assert V20 = C31    report " error in initializing S20" severity error;
    assert V21 = C32    report " error in initializing S21" severity error;
    assert V22 = C50    report " error in initializing S22" severity error;
    assert V23 = C51    report " error in initializing S23" severity error;
    assert V24 = C53     report " error in initializing S24" severity error;
    assert V25 = C70     report " error in initializing S25" severity error;
    assert V26 = C71    report " error in initializing S26" severity error;
    assert V27 = C72    report " error in initializing S27" severity error;
    assert V28 = C73    report " error in initializing S28" severity error;
    assert V29 = C74    report " error in initializing S29" severity error;
    assert V30 = C75    report " error in initializing S30" severity error;
    assert V31 = C76    report " error in initializing S31" severity error;
    assert V32 = C54a    report " error in initializing S32" severity error;
    assert V33 = C54a    report " error in initializing S33" severity error;
    assert V34= C54b    report " error in initializing S34" severity error;
    assert V35 = C55    report " error in initializing S35" severity error;
    assert V36 = C60    report " error in initializing S36" severity error;
    assert V37 = C61    report " error in initializing S37" severity error;
    assert V41= C78    report " error in initializing S41" severity error;
    assert V42= C79    report " error in initializing S42" severity error;
    assert V43= C80    report " error in initializing S43" severity error;
    assert V44= C81     report " error in initializing S44" severity error;
    assert V45= C82    report " error in initializing S45" severity error;
    assert V46= C83    report " error in initializing S46" severity error;
    assert V47= C84    report " error in initializing S47" severity error;
    assert V48= C85    report " error in initializing S48" severity error;
    assert V49= C86    report " error in initializing S49" severity error;
    assert V50= C88    report " error in initializing S50" severity error;
    assert V51= C102    report " error in initializing S51" severity error;

    assert NOT(    (V1(0) = C1)    and 
                   (V2(0) = C4)    and 
                   (V3(0) = C5)    and 
                   (V4(0) = C6)    and 
                   (V5(0) = C7)    and 
                   (V6(0) = C8)    and 
                   (V7(0) = C9)    and 
                   V8 = C19    and 
                   V9 = C20    and 
                   V10 = C21    and 
                   V11 = C22    and 
                   V12 = C23    and 
                   V13 = C24    and 
                   V14 = C25    and 
                   V15 = C26    and 
                   V16 = C27    and 
                   V17 = C28    and 
                   V18 = C29    and 
                   V19 = C30    and 
                   V20 = C31    and 
                   V21 = C32    and
                   V22 = C50    and
                   V23 = C51    and
                   V24 = C53     and
                   V25 = C70     and
                   V26 = C71    and
                   V27 = C72    and
                   V28 = C73    and
                   V29 = C74    and
                   V30 = C75    and
                   V31 = C76    and
                   V32 = C54a    and
                   V33 = C54a    and
                   V34= C54b    and
                   V35 = C55    and
                   V36 = C60    and
                   V37 = C61    and
                   V41= C78    and
                   V42= C79    and
                   V43= C80    and
                   V44= C81     and
                   V45= C82    and
                   V46= C83    and 
                   V47= C84    and 
                   V48= C85    and 
                   V49= C86    and 
                   V50= C88    and 
                   V51= C102    )   
      report "***PASSED TEST: c01s01b01x01p05n02i00749"
      severity NOTE;
    assert (    (V1(0) = C1)    and 
                (V2(0) = C4)    and 
                (V3(0) = C5)    and 
                (V4(0) = C6)    and 
                (V5(0) = C7)    and 
                (V6(0) = C8)    and 
                (V7(0) = C9)    and 
                V8 = C19    and 
                V9 = C20    and 
                V10 = C21    and 
                V11 = C22    and 
                V12 = C23    and 
                V13 = C24    and 
                V14 = C25    and 
                V15 = C26    and 
                V16 = C27    and 
                V17 = C28    and 
                V18 = C29    and 
                V19 = C30    and 
                V20 = C31    and 
                V21 = C32    and
                V22 = C50    and
                V23 = C51    and
                V24 = C53     and
                V25 = C70     and
                V26 = C71    and
                V27 = C72    and
                V28 = C73    and
                V29 = C74    and
                V30 = C75    and
                V31 = C76    and
                V32 = C54a    and
                V33 = C54a    and
                V34= C54b    and
                V35 = C55    and
                V36 = C60    and
                V37 = C61    and
                V41= C78    and
                V42= C79    and
                V43= C80    and
                V44= C81     and
                V45= C82    and
                V46= C83    and 
                V47= C84    and 
                V48= C85    and 
                V49= C86    and 
                V50= C88    and 
                V51= C102    )   
      report "***FAILED TEST: c01s01b01x01p05n02i00749 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00749arch;
