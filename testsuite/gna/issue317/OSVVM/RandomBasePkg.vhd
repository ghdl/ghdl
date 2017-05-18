--
--  File Name:         RandomBasePkg.vhd
--  Design Unit Name:  RandomBasePkg
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis      jim@synthworks.com
--
--
--  Description:
--        Defines Base randomization, seed definition, seed generation,
--        and seed IO functionality for RandomPkg.vhd
--        Defines:
--          Procedure Uniform - baseline randomization 
--          Type RandomSeedType - the seed as a single object
--          function GenRandSeed from integer_vector, integer, or string
--          IO function to_string, & procedures write, read
--
--        In revision 2.0 these types and functions are included by package reference.
--        Long term these will be passed as generics to RandomGenericPkg
--
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--  Revision History:
--    Date       Version    Description
--    01/2008:   0.1        Initial revision
--                          Numerous revisions for VHDL Testbenches and Verification
--    02/2009:   1.0        First Public Released Version
--    02/25/2009 1.1        Replaced reference to std_2008 with a reference 
--                          to ieee_proposed.standard_additions.all ;
--    03/01/2011 2.0        STANDARD VERSION
--                          Fixed abstraction by moving RandomParmType to RandomPkg.vhd 
--    4/2013     2013.04    No Changes
--    5/2013     2013.05    No Changes
--    1/2015     2015.01    Changed Assert/Report to Alert
--    6/2015     2015.06    Changed GenRandSeed to impure
--
--
--  Copyright (c) 2008 - 2015 by SynthWorks Design Inc.  All rights reserved.
--
--  Verbatim copies of this source file may be used and
--  distributed without restriction.
--
--  This source file is free software; you can redistribute it
--  and/or modify it under the terms of the ARTISTIC License
--  as published by The Perl Foundation; either version 2.0 of
--  the License, or (at your option) any later version.
--
--  This source is distributed in the hope that it will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied
--  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the Artistic License for details.
--
--  You should have received a copy of the license with this source.
--  If not download it from,
--     http://www.perlfoundation.org/artistic_license_2_0
--

library ieee ;
use ieee.math_real.all ;
use std.textio.all ;

use work.OsvvmGlobalPkg.all ; 
use work.AlertLogPkg.all ; 

-- comment out following 2 lines with VHDL-2008.  Leave in for VHDL-2002 
-- library ieee_proposed ;						          -- remove with VHDL-2008
-- use ieee_proposed.standard_additions.all ;   -- remove with VHDL-2008


package RandomBasePkg is

  -- RandomSeedType and Uniform can be replaced by any procedure that
  -- produces a uniform distribution with 0 <= Value < 1  or  0 < Value < 1
  -- and maintains the same call interface
  type RandomSeedType is array (1 to 2) of integer ;
  procedure Uniform (Result : out real ;  Seed : inout RandomSeedType) ;

  -- Translate from integer_vector, integer, or string to RandomSeedType
  -- Required by RandomPkg.InitSeed
  -- GenRandSeed makes sure all values are in a valid range
  impure function  GenRandSeed(IV : integer_vector) return RandomSeedType ;
  impure function  GenRandSeed(I : integer) return RandomSeedType ;
  impure function  GenRandSeed(S : string) return RandomSeedType ;
  
  -- IO for RandomSeedType.  If use subtype, then create aliases here
  -- in a similar fashion VHDL-2008 std_logic_textio.
  -- Not required by RandomPkg
  function  to_string(A : RandomSeedType) return string ;
  procedure write(variable L: inout line ; A : RandomSeedType ) ;
  procedure read (variable L: inout line ; A : out RandomSeedType ; good : out boolean ) ;
  procedure read (variable L: inout line ; A : out RandomSeedType ) ;

end RandomBasePkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body RandomBasePkg is

  -----------------------------------------------------------------
  -- Uniform
  --   Generate a random number with a Uniform distribution
  --   Required by RandomPkg.  All randomization is derived from here.
  --   Value produced must be either: 
  --     0 <= Value < 1  or  0 < Value < 1
  --
  --   Current version uses ieee.math_real.Uniform
  --   This abstraction allows higher precision version 
  --   of a uniform distribution to be used provided
  --
  procedure Uniform (
    Result : out   real ;
    Seed   : inout RandomSeedType 
  ) is
  begin
    ieee.math_real.Uniform (Seed(Seed'left), Seed(Seed'right), Result) ;
  end procedure Uniform ;


  -----------------------------------------------------------------
  --  GenRandSeed
  --    Convert integer_vector to RandomSeedType
  --    Uniform requires two seed values of the form:
  --        1 <= SEED1 <= 2147483562; 1 <= SEED2 <= 2147483398
  --
  --    if 2 seed values are passed to GenRandSeed and they are 
  --    in the above range, then they must remain unmodified.
  --
  impure function GenRandSeed(IV : integer_vector) return RandomSeedType is
    alias iIV : integer_vector(1 to IV'length) is IV ;
    variable Seed1 : integer ;
    variable Seed2 : integer ;
    constant SEED1_MAX : integer := 2147483562 ;
    constant SEED2_MAX : integer := 2147483398 ;
  begin
    if iIV'Length <= 0 then  -- no seed
      Alert(OSVVM_ALERTLOG_ID, "RandomBasePkg.GenRandSeed received NULL integer_vector", FAILURE) ; 
      return (3, 17) ;  -- if continue seed = (3, 17)

    elsif iIV'Length = 1 then  -- one seed value
      -- inefficient handling, but condition is unlikely
      return GenRandSeed(iIV(1)) ;  -- generate a seed

    else  -- only use the left two values
      -- 1 <= SEED1 <= 2147483562
      -- mod returns 0 to MAX-1, the -1 adjusts legal values, +1 adjusts them back
      Seed1 := ((iIV(1)-1) mod SEED1_MAX) + 1 ;
     -- 1 <= SEED2 <= 2147483398
      Seed2 := ((iIV(2)-1) mod SEED2_MAX) + 1 ;
      return (Seed1, Seed2) ;
    end if ;
  end function GenRandSeed ;


  -----------------------------------------------------------------
  --  GenRandSeed
  --    transform a single integer into the internal seed
  --
  impure function GenRandSeed(I : integer) return RandomSeedType is
    variable result : integer_vector(1 to 2) ;
  begin
    result(1) := I ;
    result(2) := I/3 + 1 ;
    return GenRandSeed(result) ; -- make value ranges legal
  end function GenRandSeed ;


  -----------------------------------------------------------------
  --  GenRandSeed
  --    transform a string value into the internal seed
  --    usage:  RV.GenRandSeed(RV'instance_path));
  --
  impure function GenRandSeed(S : string) return RandomSeedType is
    constant LEN : integer := S'length ;
    constant HALF_LEN : integer := LEN/2 ;
    alias revS : string(LEN downto 1) is S ;
    variable result : integer_vector(1 to 2) ;
    variable temp : integer := 0 ;
  begin
    for i in 1 to HALF_LEN loop
      temp := (temp + character'pos(revS(i))) mod (integer'right - 2**8) ;
    end loop ;
    result(1) := temp ;
    for i in HALF_LEN + 1 to LEN loop
      temp := (temp + character'pos(revS(i))) mod (integer'right - 2**8) ;
    end loop ;
    result(2) := temp ;
    return GenRandSeed(result) ;  -- make value ranges legal
  end function GenRandSeed ;


  -----------------------------------------------------------------
  function to_string(A : RandomSeedType) return string is
  begin
    return to_string(A(A'left)) & " " & to_string(A(A'right)) ;
  end function to_string ;


  -----------------------------------------------------------------
  procedure write(variable L: inout line ; A : RandomSeedType ) is
  begin
    write(L, to_string(A)) ;
  end procedure ;


  -----------------------------------------------------------------
  procedure read(variable L: inout line ; A : out RandomSeedType ; good : out boolean ) is
    variable iReadValid : boolean ;
  begin
    for i in A'range loop
      read(L, A(i), iReadValid) ;
      exit when not iReadValid ;
    end loop ;
    good := iReadValid ;
  end procedure read ;


  -----------------------------------------------------------------
  procedure read(variable L: inout line ; A : out RandomSeedType ) is
    variable ReadValid : boolean ;
  begin
      read(L, A, ReadValid) ;
      AlertIfNot(ReadValid, OSVVM_ALERTLOG_ID, "RandomBasePkg.read[line, RandomSeedType] failed", FAILURE) ;  
  end procedure read ;
  
end RandomBasePkg ;