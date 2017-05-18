--
--  File Name :         RandomPkg.vhd
--  Design Unit Name :  RandomPkg
--  Revision :          STANDARD VERSION
--
--  Maintainer :        Jim Lewis      email :  jim@synthworks.com
--  Contributor(s) :
--     Jim Lewis      email :  jim@synthworks.com
--     *
--
--   * In writing procedures normal, poisson, the following sources were referenced :
--     Wikipedia
--     package rnd2 written by John Breen and Ken Christensen
--     package RNG written by Gnanasekaran Swaminathan
--
--
--  Description :
--    RandomPType, a protected type, defined to hold randomization RandomSeeds and
--    function methods to facilitate randomization with uniform and weighted
--    distributions
--
--  Developed for :
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http ://www.SynthWorks.com
--
--  Revision History :
--    Date       Version    Description
--    12/2006 :  0.1        Initial revision
--                          Numerous revisions for SynthWorks' Advanced VHDL Testbenches and Verification
--    02/2009 :  1.0        First Public Released Version
--    02/25/2009 1.1        Replaced reference to std_2008 with a reference to
--                          ieee_proposed.standard_additions.all ;
--    06/2010    1.2        Added Normal and Poisson distributions
--    03/2011    2.0        Major clean-up. Moved RandomParmType and control to here
--    07/2011    2.1        Bug fix to convenience functions for slv, unsigned, and signed.
--    06/2012    2.2        Removed '_' in the name of subprograms FavorBig and FavorSmall
--    04/2013    2013.04    Changed DistInt.  Return array indices now match input
--                          Better Min, Max error handling in Uniform, FavorBig, FavorSmall, Normal, Poisson
--    5/2013     -          Removed extra variable declaration in functions RandInt and RandReal
--    5/2013     2013.05    Big vector randomization added overloading RandUnsigned, RandSlv, and RandSigned
--                          Added NULL_RANGE_TYPE to minimize null range warnings
--    1/2014     2014.01    Added RandTime, RandReal(set), RandIntV, RandRealV, RandTimeV
--                          Made sort, revsort from SortListPkg_int visible via aliases
--    1/2015     2015.01    Changed Assert/Report to Alert
--    5/2015     2015.06    Revised Alerts to Alert(OSVVM_ALERTLOG_ID, ...) ;
--    11/2016    2016.11    No changes.  Updated release numbers to make documentation and
--                          package have consistent release identifiers.
--
--  Copyright (c) 2006 - 2016 by SynthWorks Design Inc.  All rights reserved.
--
--  Verbatim copies of this source file may be used and
--  distributed without restriction.
--
--  This source file is free software ; you can redistribute it
--  and/or modify it under the terms of the ARTISTIC License
--  as published by The Perl Foundation ; either version 2.0 of
--  the License, or (at your option) any later version.
--
--  This source is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY ; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the Artistic License for details.
--
--  You should have received a copy of the license with this source.
--  If not download it from,
--     http ://www.perlfoundation.org/artistic_license_2_0
--

use work.OsvvmGlobalPkg.all ; 
use work.AlertLogPkg.all ; 
use work.RandomBasePkg.all ;
use work.SortListPkg_int.all ;

use std.textio.all ;

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;
use ieee.numeric_std_unsigned.all ;
use ieee.math_real.all ;

-- comment out following 3 lines with VHDL-2008.  Leave in for VHDL-2002
-- library ieee_proposed ;						               -- remove with VHDL-2008
-- use ieee_proposed.standard_additions.all ;        -- remove with VHDL-2008
-- use ieee_proposed.standard_textio_additions.all ; -- remove with VHDL-2008


package RandomPkg is
--  Uncomment the following with VHDL-2008 package generics.
--  For now they are defined in the package RandomBasePkg.vhd
--  package RandomGenericPkg is
  --  generic (
  --    type RandomSeedType ;    -- base type for randomization
  --    procedure Uniform (Result : out real ; Seed : inout RandomSeedType) ;
  --    function  GenRandSeed(IV : integer_vector) return RandomSeedType ;
  --    function  GenRandSeed(I : integer) return RandomSeedType ;
  --    function  GenRandSeed(S : string) return RandomSeedType ;
  --  ) ;

  -- make things from SortListPkg_int visible
  alias sort is work.SortListPkg_int.sort[integer_vector return integer_vector] ;
  alias revsort is work.SortListPkg_int.revsort[integer_vector return integer_vector] ;

  -- note NULL_RANGE_TYPE should probably be in std.standard
  subtype NULL_RANGE_TYPE is integer range 0 downto 1 ;
  constant NULL_INTV : integer_vector (NULL_RANGE_TYPE) := (others => 0) ;

  -- Supports DistValInt functionality
  type DistRecType is record
    Value  : integer ;
    Weight : integer ;
  end record ;
  type DistType is array (natural range <>) of DistRecType ;


  -- Parameters for randomization
  -- RandomDistType specifies the distribution to use for randomize
  type RandomDistType is (NONE, UNIFORM, FAVOR_SMALL, FAVOR_BIG, NORMAL, POISSON) ;

  type RandomParmType is record
    Distribution : RandomDistType ;
    Mean         : Real ; -- also used as probability of success
    StdDeviation : Real ; -- also used as number of trials for binomial
  end record ;

  -- RandomParm IO
  function to_string(A : RandomDistType) return string ;
  procedure write(variable L : inout line ; A : RandomDistType ) ;
  procedure read(variable L : inout line ; A : out RandomDistType ; good : out boolean ) ;
  procedure read(variable L : inout line ; A : out RandomDistType ) ;
  function to_string(A : RandomParmType) return string ;
  procedure write(variable L : inout line ; A : RandomParmType ) ;
  procedure read(variable L : inout line ; A : out RandomParmType ; good : out boolean ) ;
  procedure read(variable L : inout line ; A : out RandomParmType ) ;


  type RandomPType is protected
    -- Seed Manipulation
    -- Known ambiguity between InitSeed with string and integer_vector
    -- Recommendation, use :  RV.InitSeed(RV'instance_path) ;
    -- For integer_vector use either : RV.InitSeed(IV => (1,5)) ;
    --   or : RV.InitSeed(integer_vector'(1,5)) ;
    procedure InitSeed (S  : string ) ;
    procedure InitSeed (I  : integer ) ;
    procedure InitSeed (IV : integer_vector ) ;

    -- SetSeed & GetSeed :  Used to save and restore seed values
    procedure SetSeed (RandomSeedIn : RandomSeedType ) ;
    impure function GetSeed return RandomSeedType ;
    -- SeedRandom = SetSeed & GetSeed for SV compatibility
    -- replace with aliases when they work in popular simulators
    procedure SeedRandom (RandomSeedIn : RandomSeedType ) ;
    impure function SeedRandom return RandomSeedType ;
    -- alias SeedRandom is SetSeed [RandomSeedType] ;
    -- alias SeedRandom is GetSeed [return RandomSeedType] ;

    -- Setting Randomization Parameters
    -- Allows RandInt to have distributions other than uniform
    procedure SetRandomParm (RandomParmIn : RandomParmType) ;
    procedure SetRandomParm (
      Distribution : RandomDistType ;
      Mean         : Real := 0.0 ;
      Deviation    : Real := 0.0
    ) ;
    impure function GetRandomParm return RandomParmType ;
    impure function GetRandomParm return RandomDistType ;

    -- For compatibility with previous version - replace with alias
    procedure SetRandomMode (RandomDistIn : RandomDistType) ;
    -- alias SetRandomMode is SetRandomParm [RandomDistType, Real, Real] ;

    --  Base Randomization Distributions
    -- Uniform :   Generate a random number with a Uniform distribution
    impure function Uniform (Min, Max : in real) return real ;
    impure function Uniform (Min, Max : integer) return integer ;
    impure function Uniform (Min, Max : integer ; Exclude : integer_vector) return integer ;

    -- FavorSmall
    --   Generate random numbers with a greater number of small
    --   values than large values
    impure function FavorSmall (Min, Max : real) return real ;
    impure function FavorSmall (Min, Max : integer) return integer ;
    impure function FavorSmall (Min, Max : integer ; Exclude : integer_vector) return integer ;

    -- FavorBig
    --   Generate random numbers with a greater number of large
    --   values than small values
    impure function FavorBig (Min, Max : real) return real ;
    impure function FavorBig (Min, Max : integer) return integer ;
    impure function FavorBig (Min, Max : integer ; Exclude : integer_vector) return integer ;

    -- Normal :   Generate a random number with a normal distribution
    impure function Normal (Mean, StdDeviation : real) return real ;
    -- Normal + RandomVal >= Min and RandomVal < Max
    impure function Normal (Mean, StdDeviation, Min, Max : real) return real ;
    impure function Normal (
      Mean          : real ;
      StdDeviation  : real ;
      Min           : integer ;
      Max           : integer ;
      Exclude       : integer_vector := NULL_INTV
    ) return integer ;

    -- Poisson :  Generate a random number with a poisson distribution
    --   Discrete distribution = only generates integral values
    impure function Poisson (Mean : real) return real ;
    -- Poisson + RandomVal >= Min and RandomVal < Max
    impure function Poisson (Mean, Min, Max : real) return real ;
    impure function Poisson (
      Mean          : real ;
      Min           : integer ;
      Max           : integer ;
      Exclude       : integer_vector := NULL_INTV
    ) return integer ;

    -- randomization with a range
    impure function RandInt (Min, Max : integer) return integer ;
    impure function RandReal(Min, Max : Real) return real ;
    impure function RandTime (Min, Max : time ; Unit : time := ns) return time ;
    impure function RandSlv (Min, Max, Size : natural) return std_logic_vector ;
    impure function RandUnsigned (Min, Max, Size : natural) return Unsigned ;
    impure function RandSigned (Min, Max : integer ; Size : natural ) return Signed ;
    impure function RandIntV (Min, Max : integer ; Size : natural) return integer_vector ;
    impure function RandIntV (Min, Max : integer ; Unique : natural ; Size : natural) return integer_vector ;
    impure function RandRealV (Min, Max : real ; Size : natural) return real_vector ;
    impure function RandTimeV (Min, Max : time ; Size : natural ; Unit : time := ns) return time_vector ;
    impure function RandTimeV (Min, Max : time ; Unique : natural ; Size : natural ; Unit : time := ns) return time_vector ;

    --  randomization with a range and exclude vector
    impure function RandInt (Min, Max : integer ; Exclude : integer_vector ) return integer ;
    impure function RandTime (Min, Max : time ; Exclude : time_vector ; Unit : time := ns) return time ;
    impure function RandSlv (Min, Max : natural ; Exclude : integer_vector ; Size : natural ) return std_logic_vector ;
    impure function RandUnsigned (Min, Max : natural ; Exclude : integer_vector ; Size : natural ) return Unsigned ;
    impure function RandSigned (Min, Max : integer ; Exclude : integer_vector ; Size : natural ) return Signed ;
    impure function RandIntV (Min, Max : integer ; Exclude : integer_vector ; Size : natural) return integer_vector ;
    impure function RandIntV (Min, Max : integer ; Exclude : integer_vector ; Unique : natural ; Size : natural) return integer_vector ;
    impure function RandTimeV (Min, Max : time ; Exclude : time_vector ; Size : natural ; Unit : in time := ns) return time_vector ;
    impure function RandTimeV (Min, Max : time ; Exclude : time_vector ; Unique : natural ; Size : natural ; Unit : in time := ns) return time_vector ;

    -- Randomly select a value within a set of values
    impure function RandInt ( A : integer_vector ) return integer ;
    impure function RandReal ( A : real_vector ) return real ;
    impure function RandTime (A : time_vector) return time ;
    impure function RandSlv (A : integer_vector ; Size : natural) return std_logic_vector  ;
    impure function RandUnsigned (A : integer_vector ; Size : natural) return Unsigned ;
    impure function RandSigned (A : integer_vector ; Size : natural ) return Signed ;
    impure function RandIntV (A : integer_vector ; Size : natural) return integer_vector ;
    impure function RandIntV (A : integer_vector ; Unique : natural ; Size : natural) return integer_vector ;
    impure function RandRealV (A : real_vector ; Size : natural) return real_vector ;
    impure function RandRealV (A : real_vector ; Unique : natural ; Size : natural) return real_vector ;
    impure function RandTimeV (A : time_vector ; Size : natural) return time_vector ;
    impure function RandTimeV (A : time_vector ; Unique : natural ; Size : natural) return time_vector ;

    -- Randomly select a value within a set of values with exclude values (so can skip last or last n)
    impure function RandInt ( A, Exclude : integer_vector  ) return integer ;
    impure function RandReal ( A, Exclude : real_vector ) return real ;
    impure function RandTime (A, Exclude : time_vector) return time ;
    impure function RandSlv (A, Exclude : integer_vector ; Size : natural) return std_logic_vector  ;
    impure function RandUnsigned (A, Exclude : integer_vector ; Size : natural) return Unsigned ;
    impure function RandSigned (A, Exclude : integer_vector ; Size : natural ) return Signed ;
    impure function RandIntV (A, Exclude : integer_vector ; Size : natural) return integer_vector ;
    impure function RandIntV (A, Exclude : integer_vector ; Unique : natural ; Size : natural) return integer_vector ;
    impure function RandRealV (A, Exclude : real_vector ; Size : natural) return real_vector ;
    impure function RandRealV (A, Exclude : real_vector ; Unique : natural ; Size : natural) return real_vector ;
    impure function RandTimeV (A, Exclude : time_vector ; Size : natural) return time_vector ;
    impure function RandTimeV (A, Exclude : time_vector ; Unique : natural ; Size : natural) return time_vector ;

    -- Randomly select between 0 and N-1 based on the specified weight.
    -- where N = number values in weight array
    impure function DistInt ( Weight : integer_vector ) return integer ;
    impure function DistSlv ( Weight : integer_vector ; Size  : natural ) return std_logic_vector ;
    impure function DistUnsigned ( Weight : integer_vector ; Size  : natural ) return unsigned ;
    impure function DistSigned ( Weight : integer_vector ; Size  : natural ) return signed ;

    -- Distribution with just weights and with exclude values
    impure function DistInt ( Weight : integer_vector ; Exclude : integer_vector ) return integer ;
    impure function DistSlv ( Weight : integer_vector ; Exclude : integer_vector ; Size  : natural ) return std_logic_vector ;
    impure function DistUnsigned ( Weight : integer_vector ; Exclude : integer_vector ; Size  : natural ) return unsigned ;
    impure function DistSigned ( Weight : integer_vector ; Exclude : integer_vector ; Size  : natural ) return signed ;

    -- Distribution with weight and value
    impure function DistValInt ( A : DistType ) return integer ;
    impure function DistValSlv ( A : DistType ; Size  : natural) return std_logic_vector ;
    impure function DistValUnsigned ( A : DistType ; Size  : natural) return unsigned ;
    impure function DistValSigned ( A : DistType ; Size  : natural) return signed ;

    -- Distribution with weight and value and with exclude values
    impure function DistValInt ( A : DistType ; Exclude : integer_vector ) return integer ;
    impure function DistValSlv ( A : DistType ; Exclude : integer_vector ; Size  : natural) return std_logic_vector ;
    impure function DistValUnsigned ( A : DistType ; Exclude : integer_vector ; Size  : natural) return unsigned ;
    impure function DistValSigned ( A : DistType ; Exclude : integer_vector ; Size  : natural) return signed ;

    -- Large vector handling.
    impure function RandUnsigned (Size : natural) return unsigned ;
    impure function RandSlv (Size : natural) return std_logic_vector ;
    impure function RandSigned (Size : natural) return signed ;
    impure function RandUnsigned (Max : Unsigned) return unsigned ;
    impure function RandSlv (Max : std_logic_vector) return std_logic_vector ;
    impure function RandSigned (Max : signed) return signed ;
    impure function RandUnsigned (Min, Max : unsigned) return unsigned ;
    impure function RandSlv (Min, Max : std_logic_vector) return std_logic_vector ;
    impure function RandSigned (Min, Max : signed) return signed ;

    -- Convenience Functions
    impure function RandReal return real ; -- 0.0 to 1.0
    impure function RandReal(Max : Real) return real ; -- 0.0 to Max
    impure function RandInt (Max : integer) return integer ;
    impure function RandSlv (Max, Size : natural) return std_logic_vector ;
    impure function RandUnsigned (Max, Size : natural) return Unsigned ;
    impure function RandSigned (Max : integer ; Size : natural ) return Signed ;

  end protected RandomPType ;

end RandomPkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body RandomPkg is

  -----------------------------------------------------------------
  --  Local Randomization Support
  -----------------------------------------------------------------
  constant NULL_SLV : std_logic_vector (NULL_RANGE_TYPE) := (others => '0') ;
  constant NULL_UV : unsigned (NULL_RANGE_TYPE) := (others => '0') ;
  constant NULL_SV : signed (NULL_RANGE_TYPE) := (others => '0') ;

  -----------------------------------------------------------------
  -- Scale   --   Scale a value to be within a given range
  --
  function Scale (A, Min, Max : real) return real is
    variable ValRange : Real ;
  begin
    if Max >= Min then 
      ValRange := Max - Min ;
      return A * ValRange + Min ;
    else
      return real'left ;
    end if ; 
  end function Scale ;

  function Scale (A : real ; Min, Max : integer) return integer is
    variable ValRange : real ;
    variable rMin, rMax : real ;
  begin
    if Max >= Min then 
      rMin := real(Min) - 0.5 ;
      rMax := real(Max) + 0.5 ;
      ValRange := rMax - rMin ;
      return integer(round(A * ValRange + rMin)) ;
    else
      return integer'left ; 
    end if ; 
  end function Scale ;

  -- create more smaller values
  function FavorSmall (A : real) return real is
  begin
    return 1.0 - sqrt(A) ;
  end FavorSmall ;

  -- create more larger values
  -- alias FavorBig is sqrt[real return real] ;
  function FavorBig   (A : real) return real is
  begin
    return sqrt(A) ;
  end FavorBig ;

  -- local.  
  function to_time_vector (A : integer_vector ; Unit : time) return time_vector is
    variable result : time_vector(A'range) ;
  begin
    for i in A'range loop
      result(i) := A(i) * Unit ;
    end loop ;
    return result ;
  end function to_time_vector ;

  -- local
  function to_integer_vector (A : time_vector ; Unit : time) return integer_vector is
    variable result : integer_vector(A'range) ;
  begin
    for i in A'range loop
      result(i) := A(i) / Unit ;
    end loop ;
    return result ;
  end function to_integer_vector ;
  
  -- Local.  Remove the exclude list from the list - integer_vector
  procedure RemoveExclude(A, Exclude : integer_vector ; variable NewA : out integer_vector ; variable NewALength : inout natural ) is
    alias norm_NewA : integer_vector(1 to NewA'length) is NewA ;
  begin
    NewALength := 0 ;
    for i in A'range loop
      if not inside(A(i), Exclude) then
        NewALength := NewALength + 1 ;
        norm_NewA(NewALength) := A(i) ;
      end if ;
    end loop ;
  end procedure RemoveExclude ;

  -- Local.  Inside - real_vector
  function inside(A : real ; Exclude : real_vector) return boolean is
  begin
    for i in Exclude'range loop 
      if A = Exclude(i) then 
        return TRUE ; 
      end if ; 
    end loop ;
    return FALSE ;
  end function inside ; 
  
  -- Local.  Remove the exclude list from the list - real_vector
  procedure RemoveExclude(A, Exclude : real_vector ; variable NewA : out real_vector ; variable NewALength : inout natural ) is
    alias norm_NewA : real_vector(1 to NewA'length) is NewA ;
  begin
    NewALength := 0 ;
    for i in A'range loop
      if not inside(A(i), Exclude) then
        NewALength := NewALength + 1 ;
        norm_NewA(NewALength) := A(i) ;
      end if ;
    end loop ;
  end procedure RemoveExclude ;

  -- Local.  Inside - time_vector
  function inside(A : time ; Exclude : time_vector) return boolean is
  begin
    for i in Exclude'range loop 
      if A = Exclude(i) then 
        return TRUE ; 
      end if ; 
    end loop ;
    return FALSE ;
  end function inside ; 
  
  -- Local.  Remove the exclude list from the list - time_vector
  procedure RemoveExclude(A, Exclude : time_vector ; variable NewA : out time_vector ; variable NewALength : inout natural ) is
    alias norm_NewA : time_vector(1 to NewA'length) is NewA ;
  begin
    NewALength := 0 ;
    for i in A'range loop
      if not inside(A(i), Exclude) then
        NewALength := NewALength + 1 ;
        norm_NewA(NewALength) := A(i) ;
      end if ;
    end loop ;
  end procedure RemoveExclude ;


  -----------------------------------------------------------------
  --  RandomParmType IO
  -----------------------------------------------------------------
  -----------------------------------------------------------------
  function to_string(A : RandomDistType) return string is
  begin
    return RandomDistType'image(A) ;
  end function to_string ;


  -----------------------------------------------------------------
  procedure write(variable L : inout line ; A : RandomDistType ) is
  begin
    write(L, to_string(A)) ;
  end procedure write ;


  -----------------------------------------------------------------
  procedure read(variable L : inout line ; A : out RandomDistType ; good : out boolean ) is
    variable strval : string(1 to 40) ;
    variable len    : natural ;
  begin
    -- procedure SREAD (L : inout LINE ; VALUE : out STRING ; STRLEN : out NATURAL) ;
    sread(L, strval, len) ;
    A := RandomDistType'value(strval(1 to len)) ;
    good := len > 0 ;
  end procedure read ;


  -----------------------------------------------------------------
  procedure read(variable L : inout line ; A : out RandomDistType ) is
    variable ReadValid : boolean ;
  begin
      read(L, A, ReadValid) ;
      AlertIfNot( OSVVM_ALERTLOG_ID, ReadValid, "RandomPkg.read[line, RandomDistType] failed", FAILURE) ;
  end procedure read ;


  -----------------------------------------------------------------
  function to_string(A : RandomParmType) return string is
  begin
    return RandomDistType'image(A.Distribution) & " " &
           to_string(A.Mean, 2) & " " & to_string(A.StdDeviation, 2) ;
  end function to_string ;


  -----------------------------------------------------------------
  procedure write(variable L : inout line ; A : RandomParmType ) is
  begin
    write(L, to_string(A)) ;
  end procedure write ;


  -----------------------------------------------------------------
  procedure read(variable L : inout line ; A : out RandomParmType ; good : out boolean ) is
    variable strval : string(1 to 40) ;
    variable len    : natural ;
    variable igood  : boolean ;
  begin
    loop
      -- procedure SREAD (L : inout LINE ; VALUE : out STRING ; STRLEN : out NATURAL) ;
      sread(L, strval, len) ;
      A.Distribution := RandomDistType'value(strval(1 to len)) ;
      igood := len > 0 ;
      exit when not igood ;

      read(L, A.Mean, igood) ;
      exit when not igood ;

      read(L, A.StdDeviation, igood) ;
      exit ;
    end loop ;
    good := igood ;
  end procedure read ;


  -----------------------------------------------------------------
  procedure read(variable L : inout line ; A : out RandomParmType ) is
    variable ReadValid : boolean ;
  begin
      read(L, A, ReadValid) ;
      AlertIfNot( OSVVM_ALERTLOG_ID, ReadValid, "RandomPkg.read[line, RandomParmType] failed", FAILURE) ; 
  end procedure read ;



  -----------------------------------------------------------------
  -----------------------------------------------------------------
  type RandomPType is protected body
    --
    -- RandomSeed manipulation
    --
    variable RandomSeed : RandomSeedType := GenRandSeed(integer_vector'(1,7)) ;

    procedure InitSeed (S : string ) is
    begin
      RandomSeed := GenRandSeed(S) ;
    end procedure InitSeed ;

    procedure InitSeed (I : integer ) is
    begin
      RandomSeed := GenRandSeed(I) ;
    end procedure InitSeed ;

    procedure InitSeed (IV : integer_vector ) is
    begin
      RandomSeed := GenRandSeed(IV) ;
    end procedure InitSeed ;

    procedure SetSeed (RandomSeedIn : RandomSeedType ) is
    begin
      RandomSeed := RandomSeedIn ;
    end procedure SetSeed ;

    procedure SeedRandom (RandomSeedIn : RandomSeedType ) is
    begin
      RandomSeed := RandomSeedIn ;
    end procedure SeedRandom ;

    impure function GetSeed return RandomSeedType is
    begin
      return RandomSeed ;
    end function GetSeed ;

    impure function SeedRandom return RandomSeedType is
    begin
      return RandomSeed ;
    end function SeedRandom ;


    --
    --  randomization mode
    --
    variable RandomParm : RandomParmType ; -- left most values ok for init

    procedure SetRandomParm (RandomParmIn : RandomParmType) is
    begin
      RandomParm := RandomParmIn ;
    end procedure SetRandomParm ;

    procedure SetRandomParm (
      Distribution : RandomDistType ;
      Mean         : Real := 0.0 ;
      Deviation    : Real := 0.0
    ) is
    begin
      RandomParm := RandomParmType'(Distribution, Mean, Deviation) ;
    end procedure SetRandomParm ;


    impure function GetRandomParm return RandomParmType is
    begin
      return RandomParm ;
    end function GetRandomParm ;


    impure function GetRandomParm return RandomDistType is
    begin
      return RandomParm.Distribution ;
    end function GetRandomParm ;


    -- For compatibility with previous version
    procedure SetRandomMode (RandomDistIn : RandomDistType) is
    begin
      SetRandomParm(RandomDistIn) ;
    end procedure SetRandomMode ;


    --
    --  Base Randomization Distributions
    --
    --
    -- Uniform :   Generate a random number with a Uniform distribution
    --
    impure function Uniform (Min, Max : in real) return real is
      variable rRandomVal : real ;
    begin
      AlertIf (OSVVM_ALERTLOG_ID, Max < Min, "RandomPkg.Uniform: Max < Min", FAILURE) ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(rRandomVal, Min, Max) ;
    end function Uniform ;

    impure function Uniform (Min, Max : integer) return integer is
      variable rRandomVal : real ;
    begin
      AlertIf (OSVVM_ALERTLOG_ID, Max < Min, "RandomPkg.Uniform: Max < Min", FAILURE) ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(rRandomVal, Min, Max) ;
    end function Uniform ;

    impure function Uniform (Min, Max : integer ; Exclude : integer_vector) return integer is
      variable iRandomVal : integer ;
      variable ExcludeList : SortListPType ;
      variable count : integer ;
    begin
      ExcludeList.add(Exclude, Min, Max) ;
      count := ExcludeList.count ;
      iRandomVal := Uniform(Min, Max - count) ;
      -- adjust count, note iRandomVal changes while checking.
      for i in 1 to count loop
        exit when iRandomVal < ExcludeList.Get(i) ;
        iRandomVal := iRandomVal + 1 ;
      end loop ;
      ExcludeList.erase ;
      return iRandomVal ;
    end function Uniform ;


    --
    -- FavorSmall
    --   Generate random numbers with a greater number of small
    --   values than large values
    --
    impure function FavorSmall (Min, Max : real) return real is
      variable rRandomVal : real ;
    begin
      AlertIf (OSVVM_ALERTLOG_ID, Max < Min, "RandomPkg.FavorSmall: Max < Min", FAILURE) ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(FavorSmall(rRandomVal), Min, Max) ; -- real
    end function FavorSmall ;

    impure function FavorSmall (Min, Max : integer) return integer is
      variable rRandomVal : real ;
    begin
      AlertIf (OSVVM_ALERTLOG_ID, Max < Min, "RandomPkg.FavorSmall: Max < Min", FAILURE) ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(FavorSmall(rRandomVal), Min, Max) ; -- integer
    end function FavorSmall ;

    impure function FavorSmall (Min, Max : integer ; Exclude : integer_vector) return integer is
      variable iRandomVal : integer ;
      variable ExcludeList : SortListPType ;
      variable count : integer ;
    begin
      ExcludeList.add(Exclude, Min, Max) ;
      count := ExcludeList.count ;
      iRandomVal := FavorSmall(Min, Max - count) ;
      -- adjust count, note iRandomVal changes while checking.
      for i in 1 to count loop
        exit when iRandomVal < ExcludeList.Get(i) ;
        iRandomVal := iRandomVal + 1 ;
      end loop ;
      ExcludeList.erase ;
      return iRandomVal ;
    end function FavorSmall ;


    --
    -- FavorBig
    --   Generate random numbers with a greater number of large
    --   values than small values
    --
    impure function FavorBig (Min, Max : real) return real is
      variable rRandomVal : real ;
    begin
      AlertIf (OSVVM_ALERTLOG_ID, Max < Min, "RandomPkg.FavorBig: Max < Min", FAILURE) ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(FavorBig(rRandomVal), Min, Max) ; -- real
    end function FavorBig ;

    impure function FavorBig (Min, Max : integer) return integer is
      variable rRandomVal : real ;
    begin
      AlertIf (OSVVM_ALERTLOG_ID, Max < Min, "RandomPkg.FavorBig: Max < Min", FAILURE) ;
      Uniform(rRandomVal, RandomSeed) ;
      return scale(FavorBig(rRandomVal), Min, Max) ; -- integer
    end function FavorBig ;

    impure function FavorBig (Min, Max : integer ; Exclude : integer_vector) return integer is
      variable iRandomVal : integer ;
      variable ExcludeList : SortListPType ;
      variable count : integer ;
    begin
      ExcludeList.add(Exclude, Min, Max) ;
      count := ExcludeList.count ;
      iRandomVal := FavorBig(Min, Max - count) ;
      -- adjust count, note iRandomVal changes while checking.
      for i in 1 to count loop
        exit when iRandomVal < ExcludeList.Get(i) ;
        iRandomVal := iRandomVal + 1 ;
      end loop ;
      ExcludeList.erase ;
      return iRandomVal ;
    end function FavorBig ;


    -----------------------------------------------------------------
    -- Normal
    --   Generate a random number with a normal distribution
    --
    -- Use Box Muller, per Wikipedia :
    --    http ://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
    --
    -- Use polar method, per Wikipedia :
    --   http ://en.wikipedia.org/wiki/Marsaglia_polar_method
    --
    impure function Normal (Mean, StdDeviation : real) return real is
      variable x01, y01 : real ;
      variable StdNormalDist : real ; -- mean 0, variance 1
    begin
      -- add this check to set parameters?
      if StdDeviation < 0.0 then
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.Normal: Standard deviation must be >= 0.0", FAILURE) ;
        return -1.0 ;
      end if ;

      -- Box Muller
      Uniform (x01, RandomSeed) ;
      Uniform (y01, RandomSeed) ;
      StdNormalDist := sqrt(-2.0 * log(x01)) * cos(math_2_pi*y01) ;

      -- Polar form rejected due to mean 50.0, std deviation = 5 resulted
      -- in a median of 49
      -- -- find two Uniform distributed values with range -1 to 1
      -- -- that satisify S = X **2 + Y**2 < 1.0
      -- loop
        -- Uniform (x01, RandomSeed) ;
        -- Uniform (y01, RandomSeed) ;
        -- x := 2.0 * x01 - 1.0 ; -- scale to -1 to 1
        -- y := 2.0 * y01 - 1.0 ;
        -- s := x*x + y*y ;
        -- exit when s < 1.0 and s > 0.0 ;
      -- end loop ;

      -- -- Calculate Standard Normal Distribution
      -- StdNormalDist := x * sqrt((-2.0 * log(s)) / s) ;

      -- Convert to have Mean and StdDeviation
      return StdDeviation * StdNormalDist + Mean ;
    end function Normal ;


    -- Normal + RandomVal >= Min and RandomVal <= Max
    impure function Normal (Mean, StdDeviation, Min, Max : real) return real is
      variable rRandomVal : real ;
    begin
      if Max < Min then
         Alert(OSVVM_ALERTLOG_ID, "RandomPkg.Normal: Max < Min", FAILURE) ;
         return Mean ; 
      else
        loop
          rRandomVal := Normal (Mean, StdDeviation) ;
          exit when rRandomVal >= Min and rRandomVal <= Max ;
        end loop ;
      end if ;
      return rRandomVal ;
    end function Normal ;

    -- Normal + RandomVal >= Min and RandomVal <= Max
    impure function Normal (
      Mean          : real ;
      StdDeviation  : real ;
      Min           : integer ;
      Max           : integer ;
      Exclude       : integer_vector := NULL_INTV
    ) return integer is
      variable iRandomVal : integer ;
    begin
      if Max < Min then
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.Normal: Max < Min", FAILURE) ;
        return integer(round(Mean)) ;
      else
        loop
          iRandomVal := integer(round(  Normal(Mean, StdDeviation)  )) ;
          exit when iRandomVal >= Min and iRandomVal <= Max and
                     not inside(iRandomVal, Exclude) ;
        end loop ;
      end if ;
      return iRandomVal ;
    end function Normal ;


    -----------------------------------------------------------------
    -- Poisson
    --   Generate a random number with a poisson distribution
    --   Discrete distribution = only generates integral values
    --
    -- Use knuth method, per Wikipedia :
    --   http ://en.wikipedia.org/wiki/Poisson_distribution
    --
    impure function Poisson (Mean : real) return real is
      variable Product      : Real := 1.0 ;
      variable Bound        : Real := 0.0 ;
      variable UniformRand  : Real := 0.0 ;
      variable PoissonRand  : Real := 0.0 ;
    begin
      Bound := exp(-1.0 * Mean) ;
      Product := 1.0 ;

      -- add this check to set parameters?
      if Mean <= 0.0 or Bound <= 0.0 then
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.Poisson: Mean < 0 or too large.  Mean = " & real'image(Mean), FAILURE) ;
        return Mean ;
      end if ;

      while (Product >= Bound) loop
        PoissonRand := PoissonRand + 1.0 ;
        Uniform(UniformRand, RandomSeed) ;
        Product := Product * UniformRand ;
      end loop ;
      return PoissonRand ;
    end function  Poisson ; -- no range

    -- Poisson + RandomVal >= Min and RandomVal < Max
    impure function Poisson (Mean, Min, Max : real) return real is
      variable rRandomVal : real ;
    begin
      if Max < Min then
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.Poisson: Max < Min", FAILURE) ;
        return Mean ; 
      else
        loop
          rRandomVal := Poisson (Mean) ;
          exit when rRandomVal >= Min and rRandomVal <= Max ;
        end loop ;
      end if ;
      return rRandomVal ;
    end function  Poisson ;

    impure function Poisson (
      Mean          : real ;
      Min           : integer ;
      Max           : integer ;
      Exclude       : integer_vector := NULL_INTV
    ) return integer is
      variable iRandomVal : integer ;
    begin
      if Max < Min then
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.Poisson: Max < Min", FAILURE) ;
        return integer(round(Mean)) ; 
      else
        loop
          iRandomVal := integer(round(  Poisson (Mean)  )) ;
          exit when iRandomVal >= Min and iRandomVal <= Max and
                     not inside(iRandomVal, Exclude) ;
        end loop ;
      end if ;
      return iRandomVal ;
    end function  Poisson ;
    

    --
    --  integer randomization with a range
    --    Distribution determined by RandomParm
    --
    impure function RandInt (Min, Max : integer) return integer is
    begin
      case RandomParm.Distribution is
        when NONE | UNIFORM =>  return Uniform(Min, Max) ;
        when FAVOR_SMALL  =>    return FavorSmall(Min, Max) ;
        when FAVOR_BIG    =>    return FavorBig (Min, Max) ;
        when NORMAL =>          return Normal(RandomParm.Mean, RandomParm.StdDeviation, Min, Max) ;
        when POISSON =>         return Poisson(RandomParm.Mean, Min, Max) ;
        when others =>
          Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandInt: RandomParm.Distribution not implemented", FAILURE) ;
          return integer'low ;
      end case ;
    end function RandInt ;

    --
    --  real randomization with a range
    --    Distribution determined by RandomParm
    --
    impure function RandReal(Min, Max : Real) return real is
    begin
      case RandomParm.Distribution is
        when NONE | UNIFORM =>  return Uniform(Min, Max) ;
        when FAVOR_SMALL  =>    return FavorSmall(Min, Max) ;
        when FAVOR_BIG    =>    return FavorBig (Min, Max) ;
        when NORMAL =>          return Normal(RandomParm.Mean, RandomParm.StdDeviation, Min, Max) ;
        when POISSON =>         return Poisson(RandomParm.Mean, Min, Max) ;
        when others =>
          Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandReal: Specified RandomParm.Distribution not implemented", FAILURE) ;
          return real(integer'low) ;
      end case ;
    end function RandReal ;

    impure function RandTime (Min, Max : time ; Unit :time := ns) return time is
      variable IntVal : integer ;
    begin
      --  if Max - Min > 2**31 result will be out of range
      IntVal := RandInt(0, (Max - Min)/Unit) ;
      Return Min + Unit*IntVal ;
    end function RandTime ;

    impure function RandSlv (Min, Max, Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(Min, Max), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (Min, Max, Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(Min, Max), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (Min, Max : integer ; Size : natural ) return Signed is
    begin
      return to_signed(RandInt(Min, Max), Size) ;
    end function RandSigned ;

    impure function RandIntV (Min, Max : integer ; Size : natural) return integer_vector is
      variable result : integer_vector(1 to Size) ;
    begin
      for i in result'range loop
        result(i) := RandInt(Min, Max) ;
      end loop ;
      return result ;
    end function RandIntV ;

    impure function RandIntV (Min, Max : integer ; Unique : natural ; Size : natural) return integer_vector is
      variable result : integer_vector(1 to Size) ;
      variable iUnique : natural ; 
    begin
      -- if Unique = 0, it is more efficient to call RandIntV(Min, Max, Size)
      iUnique := Unique ; 
      if Max-Min+1 < Unique then
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.(RandIntV | RandRealV | RandTimeV): Unique > number of values available", FAILURE) ;
        iUnique := Max-Min+1 ; 
      end if ; 
      for i in result'range loop
        result(i) := RandInt(Min, Max, result(maximum(1, 1 + i - iUnique) to Size)) ;
      end loop ;
      return result ;
    end function RandIntV ;

    impure function RandRealV (Min, Max : real ; Size : natural) return real_vector is
      variable result : real_vector(1 to Size) ;
    begin
      for i in result'range loop
        result(i) := RandReal(Min, Max) ;
      end loop ;
      return result ;
    end function RandRealV ;

    impure function RandTimeV (Min, Max : time ; Size : natural ; Unit : time := ns) return time_vector is
      variable result : time_vector(1 to Size) ;
    begin
      for i in result'range loop
        result(i) := RandTime(Min, Max, Unit) ;
      end loop ;
      return result ;
    end function RandTimeV ;
    
    impure function RandTimeV (Min, Max : time ; Unique : natural ; Size : natural ; Unit : time := ns) return time_vector is
    begin
      -- if Unique = 0, it is more efficient to call RandTimeV(Min, Max, Size)
      return to_time_vector(RandIntV(Min/Unit, Max/Unit, Unique, Size), Unit) ; 
    end function RandTimeV ;


    --
    --  integer randomization with a range and exclude vector
    --    Distribution determined by RandomParm
    --
    impure function RandInt (Min, Max : integer ; Exclude : integer_vector ) return integer is
    begin
      case RandomParm.Distribution is
        when NONE | UNIFORM =>  return  Uniform(Min, Max, Exclude) ;
        when FAVOR_SMALL  =>    return  FavorSmall(Min, Max, Exclude) ;
        when FAVOR_BIG    =>    return  FavorBig (Min, Max, Exclude) ;
        when NORMAL =>          return  Normal(RandomParm.Mean, RandomParm.StdDeviation, Min, Max, Exclude) ;
        when POISSON =>         return  Poisson(RandomParm.Mean, Min, Max, Exclude) ;
        when others =>
          Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandInt: Specified RandomParm.Distribution not implemented", FAILURE) ;
          return integer'low ;
      end case ;
    end function RandInt ;

    impure function RandTime (Min, Max : time ; Exclude : time_vector ; Unit : time := ns) return time is
      variable IntVal : integer ;
    begin
      --  if Min or Max > 2**31 value will be out of range
      return RandInt(Min/Unit, Max/Unit, to_integer_vector(Exclude, Unit)) * Unit ;
    end function RandTime ;

    impure function RandSlv (Min, Max : natural ; Exclude : integer_vector ; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(Min, Max, Exclude), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (Min, Max : natural ; Exclude : integer_vector ; Size  : natural ) return Unsigned is
    begin
      return to_unsigned(RandInt(Min, Max, Exclude), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (Min, Max : integer ; Exclude : integer_vector ; Size  : natural ) return Signed is
    begin
      return to_signed(RandInt(Min, Max, Exclude), Size) ;
    end function RandSigned ;

    impure function RandIntV (Min, Max : integer ; Exclude : integer_vector ; Size : natural) return integer_vector is
      variable result : integer_vector(1 to Size) ;
    begin
      for i in result'range loop
        result(i) := RandInt(Min, Max, Exclude) ;
      end loop ;
      return result ;
    end function RandIntV ;

    impure function RandIntV (Min, Max : integer ; Exclude : integer_vector ; Unique : natural ; Size : natural) return integer_vector is
      variable ResultPlus : integer_vector(1 to Size + Exclude'length) ;
    begin
      -- if Unique = 0, it is more efficient to call RandIntV(Min, Max, Size)
      ResultPlus(Size+1 to ResultPlus'right) := Exclude ;
      for i in 1 to Size loop
        ResultPlus(i) := RandInt(Min, Max, ResultPlus(maximum(1, 1 + i - Unique) to ResultPlus'right)) ;
      end loop ;
      return ResultPlus(1 to Size) ;
    end function RandIntV ;

    impure function RandTimeV (Min, Max : time ; Exclude : time_vector ; Size : natural ; Unit : in time := ns) return time_vector is
    begin
      return to_time_vector( RandIntV(Min/Unit, Max/Unit, to_integer_vector(Exclude, Unit), Size), Unit ) ; 
   end function RandTimeV ;

    impure function RandTimeV (Min, Max : time ; Exclude : time_vector ; Unique : natural ; Size : natural ; Unit : in time := ns) return time_vector is
    begin
      -- if Unique = 0, it is more efficient to call RandIntV(Min, Max, Size)
      return to_time_vector( RandIntV(Min/Unit, Max/Unit, to_integer_vector(Exclude, Unit), Unique, Size), Unit ) ; 
    end function RandTimeV ;



    --
    -- Randomly select a value within a set of values
    --    Distribution determined by RandomParm
    --
    impure function RandInt ( A : integer_vector ) return integer is
      alias A_norm : integer_vector(1 to A'length) is A ;
    begin
      return A_norm( RandInt(1, A'length) ) ;
    end function RandInt ;

    impure function RandReal ( A : real_vector ) return real is
      alias A_norm : real_vector(1 to A'length) is A ;
    begin
      return A_norm( RandInt(1, A'length) ) ;
    end function RandReal ;

    impure function RandTime ( A : time_vector ) return time is
      alias A_norm : time_vector(1 to A'length) is A ;
    begin
      return A_norm( RandInt(1, A'length) ) ;
    end function RandTime ;

    impure function RandSlv (A : integer_vector ; Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(A), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (A : integer_vector ; Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(A), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (A : integer_vector ; Size : natural ) return Signed is
    begin
      return to_signed(RandInt(A), Size) ;
    end function RandSigned ;

    impure function RandIntV (A : integer_vector ; Size : natural) return integer_vector is
      variable result : integer_vector(1 to Size) ;
    begin
      for i in result'range loop
        result(i) := RandInt(A) ;
      end loop ;
      return result ;
    end function RandIntV ;

    impure function RandIntV (A : integer_vector ; Unique : natural ; Size : natural) return integer_vector is
      variable result : integer_vector(1 to Size) ;
      variable iUnique : natural ; 
    begin
      -- if Unique = 0, it is more efficient to call RandIntV(A, Size)
      -- require A'length >= Unique
      iUnique := Unique ; 
      if A'length < Unique then
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandIntV: Unique > length of set of values", FAILURE) ;
        iUnique := A'length ; 
      end if ; 
      for i in result'range loop
        result(i) := RandInt(A, result(maximum(1, 1 + i - iUnique) to Size)) ;
      end loop ;
      return result ;
    end function RandIntV ;

    impure function RandRealV (A : real_vector ; Size : natural) return real_vector is
      variable result : real_vector(1 to Size) ;
    begin
      for i in result'range loop
        result(i) := RandReal(A) ;
      end loop ;
      return result ;
    end function RandRealV ;

    impure function RandRealV (A : real_vector ; Unique : natural ; Size : natural) return real_vector is
      alias A_norm : real_vector(1 to A'length) is A ;
      variable result : real_vector(1 to Size) ;
      variable IntResult : integer_vector(result'range) ;
    begin
      -- randomly generate indices
      IntResult := RandIntV(1, A'length, Unique, Size) ;
      -- translate indicies into result values
      for i in result'range loop
        result(i) := A_norm(IntResult(i)) ;
      end loop ;
      return result ;
    end function RandRealV ;

    impure function RandTimeV (A : time_vector ; Size : natural) return time_vector is
      variable result : time_vector(1 to Size) ;
    begin
      for i in result'range loop
        result(i) := RandTime(A) ;
      end loop ;
      return result ;
    end function RandTimeV ;

    impure function RandTimeV (A : time_vector ; Unique : natural ; Size : natural) return time_vector is
      alias A_norm : time_vector(1 to A'length) is A ;
      variable result : time_vector(1 to Size) ;
      variable IntResult : integer_vector(result'range) ;
    begin
      -- randomly generate indices
      IntResult := RandIntV(1, A'length, Unique, Size) ;
      -- translate indicies into result values
      for i in result'range loop
        result(i) := A_norm(IntResult(i)) ;
      end loop ;
      return result ;
    end function RandTimeV ;


    --
    --  Randomly select a value within a set of values with exclude values (so can skip last or last n)
    --    Distribution determined by RandomParm
    --

    impure function RandInt ( A, Exclude : integer_vector ) return integer is
      variable NewA : integer_vector(1 to A'length) ;
      variable NewALength : natural ;
    begin
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Randomize Index
      return NewA(RandInt(1, NewALength)) ;
    end function RandInt ;

    impure function RandReal ( A, Exclude : real_vector ) return real is
      variable NewA : real_vector(1 to A'length) ;
      variable NewALength : natural ;
    begin
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Randomize Index
      return NewA(RandInt(1, NewALength)) ;
    end function RandReal ;

    impure function RandTime ( A, Exclude : time_vector ) return time is
      variable NewA : time_vector(1 to A'length) ;
      variable NewALength : natural ;
    begin
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Randomize Index
      return NewA(RandInt(1, NewALength)) ;
    end function RandTime ;

    impure function RandSlv (A, Exclude : integer_vector ; Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(A, Exclude), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (A, Exclude : integer_vector ; Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(A, Exclude), Size) ;
    end function RandUnsigned ;

    impure function RandSigned (A, Exclude : integer_vector ; Size : natural ) return Signed is
    begin
      return to_signed(RandInt(A, Exclude), Size) ;
    end function RandSigned ;

    impure function RandIntV (A, Exclude : integer_vector ; Size : natural) return integer_vector is
      variable result : integer_vector(1 to Size) ;
      variable NewA  : integer_vector(1 to A'length) ;
      variable NewALength : natural ;
    begin
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Randomize Index
      for i in result'range loop
        result(i) := NewA(RandInt(1, NewALength)) ;
      end loop ;
      return result ;
    end function RandIntV ;

    impure function RandIntV (A, Exclude : integer_vector ; Unique : natural ; Size : natural) return integer_vector is
      variable result : integer_vector(1 to Size) ;
      variable NewA  : integer_vector(1 to A'length) ;
      variable NewALength, iUnique : natural ;
    begin
      -- if Unique = 0, it is more efficient to call RandIntV(Min, Max, Size)
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Require NewALength >= Unique
      iUnique := Unique ; 
      if NewALength < Unique then 
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandIntV: Unique > Length of Set A - Exclude", FAILURE) ;
        iUnique := NewALength ; 
      end if ; 
      -- Randomize using exclude list of Unique # of newly generated values
      for i in result'range loop
        result(i) := RandInt(NewA(1 to NewALength), result(maximum(1, 1 + i - iUnique) to Size)) ;
      end loop ;
      return result ;
    end function RandIntV ;

    impure function RandRealV (A, Exclude : real_vector ; Size : natural) return real_vector is
      variable result : real_vector(1 to Size) ;
      variable NewA  : real_vector(1 to A'length) ;
      variable NewALength : natural ;
    begin
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Randomize Index
      for i in result'range loop
        result(i) := NewA(RandInt(1, NewALength)) ;
      end loop ;
      return result ;
    end function RandRealV ;

    impure function RandRealV (A, Exclude : real_vector ; Unique : natural ; Size : natural) return real_vector is
      variable result : real_vector(1 to Size) ;
      variable NewA  : real_vector(1 to A'length) ;
      variable NewALength, iUnique : natural ;
    begin
      -- if Unique = 0, it is more efficient to call RandRealV(Min, Max, Size)
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Require NewALength >= Unique
      iUnique := Unique ; 
      if NewALength < Unique then 
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandRealV: Unique > Length of Set A - Exclude", FAILURE) ;
        iUnique := NewALength ; 
      end if ; 
      -- Randomize using exclude list of Unique # of newly generated values
      for i in result'range loop
        result(i) := RandReal(NewA(1 to NewALength), result(maximum(1, 1 + i - iUnique) to Size)) ;
      end loop ;
      return result ;
    end function RandRealV ;

    impure function RandTimeV (A, Exclude : time_vector ; Size : natural) return time_vector is
      variable result : time_vector(1 to Size) ;
      variable NewA  : time_vector(1 to A'length) ;
      variable NewALength : natural ;
    begin
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Randomize Index
      for i in result'range loop
        result(i) := NewA(RandInt(1, NewALength)) ;
      end loop ;
      return result ;
    end function RandTimeV ;

    impure function RandTimeV (A, Exclude : time_vector ; Unique : natural ; Size : natural) return time_vector is
      variable result : time_vector(1 to Size) ;
      variable NewA  : time_vector(1 to A'length) ;
      variable NewALength, iUnique : natural ;
    begin
      -- if Unique = 0, it is more efficient to call RandRealV(Min, Max, Size)
      -- Remove Exclude from A
      RemoveExclude(A, Exclude, NewA, NewALength) ;
      -- Require NewALength >= Unique
      iUnique := Unique ; 
      if NewALength < Unique then 
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandTimeV: Unique > Length of Set A - Exclude", FAILURE) ;
        iUnique := NewALength ; 
      end if ; 
      -- Randomize using exclude list of Unique # of newly generated values
      for i in result'range loop
        result(i) := RandTime(NewA(1 to NewALength), result(maximum(1, 1 + i - iUnique) to Size)) ;
      end loop ;
      return result ;
    end function RandTimeV ;


    --
    --  Basic Discrete Distributions
    --    Always uses Uniform
    --
    impure function DistInt ( Weight : integer_vector ) return integer is
      variable DistArray : integer_vector(weight'range) ;
      variable sum : integer ;
      variable iRandomVal : integer ;
    begin
      DistArray := Weight ;
      sum := 0 ;
      for i in DistArray'range loop
        DistArray(i) := DistArray(i) + sum ;
        if DistArray(i) < sum then
          Alert(OSVVM_ALERTLOG_ID, "RandomPkg.DistInt: negative weight or sum > 31 bits", FAILURE) ;
          return DistArray'low ; -- allows debugging vs integer'left, out of range
        end if ;
        sum := DistArray(i) ;
      end loop ;
      if sum >= 1 then
        iRandomVal := Uniform(1, sum) ;
        for i in DistArray'range loop
          if iRandomVal <= DistArray(i) then
            return i ;
          end if ;
        end loop ;
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.DistInt: randomization failed", FAILURE) ;
      else
        Alert(OSVVM_ALERTLOG_ID, "RandomPkg.DistInt: No randomization weights", FAILURE) ;
      end if ;
      return DistArray'low ; -- allows debugging vs integer'left, out of range
    end function DistInt ;

    impure function DistSlv ( Weight : integer_vector ; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(DistInt(Weight), Size)) ;
    end function DistSlv ;

    impure function DistUnsigned ( Weight : integer_vector ; Size  : natural ) return unsigned is
    begin
      return to_unsigned(DistInt(Weight), Size) ;
    end function DistUnsigned ;

    impure function DistSigned ( Weight : integer_vector ; Size  : natural ) return signed is
    begin
      return to_signed(DistInt(Weight), Size) ;
    end function DistSigned ;


    --
    --  Basic Distributions with exclude values (so can skip last or last n)
    --    Always uses Uniform via DistInt
    --
    impure function DistInt ( Weight : integer_vector ; Exclude : integer_vector ) return integer is
      variable DistArray : integer_vector(weight'range) ;
      variable ExcludeTemp : integer ;
    begin
      DistArray := Weight ;
      for i in Exclude'range loop
        ExcludeTemp := Exclude(i) ;
        if ExcludeTemp >= DistArray'low and ExcludeTemp <= DistArray'high then
          DistArray(ExcludeTemp) := 0 ;
        end if ;
      end loop ;
      return DistInt(DistArray) ;
    end function DistInt ;

    impure function DistSlv ( Weight : integer_vector ; Exclude : integer_vector ; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(DistInt(Weight, Exclude), Size)) ;
    end function DistSlv ;

    impure function DistUnsigned ( Weight : integer_vector ; Exclude : integer_vector ; Size  : natural ) return unsigned is
    begin
      return to_unsigned(DistInt(Weight, Exclude), Size) ;
    end function DistUnsigned ;

    impure function DistSigned ( Weight : integer_vector ; Exclude : integer_vector ; Size  : natural ) return signed is
    begin
      return to_signed(DistInt(Weight, Exclude), Size) ;
    end function DistSigned ;


    --
    --  Distribution for sparse values
    --    Always uses Uniform via DistInt
    --
    impure function DistValInt ( A : DistType ) return integer is
      variable DistArray : integer_vector(0 to A'length -1) ;
      alias DistRecArray : DistType(DistArray'range) is A ;
    begin
      for i in DistArray'range loop
        DistArray(i) := DistRecArray(i).Weight ;
      end loop ;
      return DistRecArray(DistInt(DistArray)).Value ;
    end function DistValInt ;

    impure function DistValSlv ( A : DistType ; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(DistValInt(A), Size)) ;
    end function DistValSlv ;

    impure function DistValUnsigned ( A : DistType ; Size  : natural ) return unsigned is
    begin
      return to_unsigned(DistValInt(A), Size) ;
    end function DistValUnsigned ;

    impure function DistValSigned ( A : DistType ; Size  : natural ) return signed is
    begin
      return to_signed(DistValInt(A), Size) ;
    end function DistValSigned ;


    --
    --  Distribution for sparse values with exclude values (so can skip last or last n)
    --    Always uses Uniform via DistInt
    --
    impure function DistValInt ( A : DistType ; Exclude : integer_vector ) return integer is
      variable DistArray : integer_vector(0 to A'length -1) ;
      alias DistRecArray : DistType(DistArray'range) is A ;
    begin
      for i in DistRecArray'range loop
        if inside(DistRecArray(i).Value, exclude) then
          DistArray(i) := 0 ; -- exclude
        else
          DistArray(i) := DistRecArray(i).Weight ;
        end if ;
      end loop ;
      return DistRecArray(DistInt(DistArray)).Value ;
    end function DistValInt ;

    impure function DistValSlv ( A : DistType ; Exclude : integer_vector ; Size  : natural ) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(DistValInt(A, Exclude), Size)) ;
    end function DistValSlv ;

    impure function DistValUnsigned ( A : DistType ; Exclude : integer_vector ; Size  : natural ) return unsigned is
    begin
      return to_unsigned(DistValInt(A, Exclude), Size) ;
    end function DistValUnsigned ;

    impure function DistValSigned ( A : DistType ; Exclude : integer_vector ; Size  : natural ) return signed is
    begin
      return to_signed(DistValInt(A, Exclude), Size) ;
    end function DistValSigned ;


    --
    -- Large vector handling.
    --
    impure function RandUnsigned (Size : natural) return unsigned is
      constant NumLoops : integer := integer(ceil(real(Size)/30.0)) ;
      constant Remain : integer := (Size - 1) mod 30 + 1 ; -- range 1 to 30
      variable RandVal : unsigned(1 to Size) ;
    begin
      if size = 0 then
        return NULL_UV ; -- Null array
      end if ;
      for i in 0 to NumLoops-2 loop
        RandVal(1 + 30*i to 30 + 30*i) := to_unsigned(RandInt(0, 2**30-1), 30) ;
      end loop ;
      RandVal(1+30*(NumLoops-1) to Remain + 30*(NumLoops-1)) := to_unsigned(RandInt(0, 2**Remain-1), Remain) ;
      return RandVal ;
    end function RandUnsigned ;

    impure function RandSlv (Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(RandUnsigned(Size)) ;
    end function RandSlv ;

    impure function RandSigned (Size : natural) return signed is
    begin
      return signed(RandUnsigned(Size)) ;
    end function RandSigned ;


    impure function RandUnsigned (Max : unsigned) return unsigned is
      alias normMax : unsigned (Max'length downto 1) is Max ;
      variable Result : unsigned(Max'range) := (others => '0') ;
      alias normResult : unsigned(normMax'range) is Result ;
      variable Size : integer ;
    begin
      -- Size = -1 if not found or Max'length = 0
      Size := find_leftmost(normMax, '1') ;

      if Size > 0 then
        loop
          normResult(Size downto 1) := RandUnsigned(Size) ;
          exit when normResult <= Max ;
        end loop ;
        return Result ; -- = normResult with range same as Max
      else
        return resize("0", Max'length) ;
      end if ;
    end function RandUnsigned ;

    -- Working version that scales the value
    -- impure function RandUnsigned (Max : unsigned) return unsigned is
    --   constant MaxVal  : unsigned(Max'length+3 downto 1) := (others => '1') ;
    -- begin
    --   if max'length > 0 then
    --     -- "Max'length+3" creates 3 guard bits
    --     return resize( RandUnsigned(Max'length+3) * ('0'&Max+1) / ('0'&MaxVal+1), Max'length) ;
    --   else
    --     return NULL_UV ; -- Null Array
    --   end if ;
    -- end function RandUnsigned ;

    impure function RandSlv (Max : std_logic_vector) return std_logic_vector is
    begin
      return std_logic_vector(RandUnsigned( unsigned(Max))) ;
    end function RandSlv ;

    impure function RandSigned (Max : signed) return signed is
    begin
      if max'length > 0 then
        AlertIf (OSVVM_ALERTLOG_ID, Max < 0, "RandomPkg.RandSigned: Max < 0", FAILURE) ;
        return signed(RandUnsigned( unsigned(Max))) ;
      else
        return NULL_SV ; -- Null Array
      end if ;
    end function RandSigned ;


    impure function RandUnsigned (Min, Max : unsigned) return unsigned is
      constant LEN : integer := maximum(Max'length, Min'length) ;
    begin
      if LEN > 0 and Min <= Max then
        return RandUnsigned(Max-Min) + Min ;
      else
        if Len > 0 then
          Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandUnsigned: Max < Min", FAILURE) ;
        end if ;
        return NULL_UV ;
      end if ;
    end function RandUnsigned ;


    impure function RandSlv (Min, Max : std_logic_vector) return std_logic_vector is
      constant LEN : integer := maximum(Max'length, Min'length) ;
    begin
      if LEN > 0 and Min <= Max then
        return RandSlv(Max-Min) + Min ;
      else
        if Len > 0 then
          Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandSlv: Max < Min", FAILURE) ;
        end if ;
          return NULL_SlV ;
      end if ;
    end function RandSlv ;


    impure function RandSigned (Min, Max : signed) return signed is
      constant LEN : integer := maximum(Max'length, Min'length) ;
    begin
      if LEN > 0 and Min <= Max then
        return resize(RandSigned(resize(Max,LEN+1) - resize(Min,LEN+1)) + Min, LEN) ;
      else
        if Len > 0 then
          Alert(OSVVM_ALERTLOG_ID, "RandomPkg.RandSigned: Max < Min", FAILURE) ;
        end if ;
        return NULL_SV ;
      end if ;
    end function RandSigned ;


    --
    -- Convenience Functions.  Resolve into calls into the other functions
    --
    impure function RandReal return real is
    begin
      return RandReal(0.0, 1.0) ;
    end function RandReal ;

    impure function RandReal(Max : Real) return real is  -- 0.0 to Max
    begin
      return RandReal(0.0, Max) ;
    end function RandReal ;

    impure function RandInt (Max : integer) return integer is
    begin
      return RandInt(0, Max) ;
    end function RandInt ;

    impure function RandSlv (Max, Size : natural) return std_logic_vector is
    begin
      return std_logic_vector(to_unsigned(RandInt(0, Max), Size)) ;
    end function RandSlv ;

    impure function RandUnsigned (Max, Size : natural) return Unsigned is
    begin
      return to_unsigned(RandInt(0, Max), Size) ;
    end function RandUnsigned ;


    impure function RandSigned (Max : integer ; Size : natural ) return Signed is
    begin
      -- chose 0 to Max rather than -Max to +Max to be same as RandUnsigned, either seems logical
      return to_signed(RandInt(0, Max), Size) ;
    end function RandSigned ;

  end protected body RandomPType ;

end RandomPkg ;