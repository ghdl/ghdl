--
--  File Name:         ResolutionPkg.vhd
--  Design Unit Name:  ResolutionPkg
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@SynthWorks.com 
--  Contributor(s):            
--     Jim Lewis      email:  jim@SynthWorks.com   
--
--  Package Defines
--      resolved resolution functions for integer, real, and time
--      types resolved_integer, resolved_real, resolved_time
--    
--  Developed for: 
--        SynthWorks Design Inc. 
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--  Revision History:
--    Date      Version    Description
--    09/2006:  0.1        Initial revision
--                         Numerous revisions for VHDL Testbenches and Verification
--    02/2009:  1.0        VHDL-2008 STANDARD VERSION
--    05/2015   2015.05    Added Alerts
--    --                   Replaced Alerts with asserts as alerts are illegal in pure functions
--    11/2016   2016.11    Removed Asserts as they are not working as intended.
--                         See ResolutionPkg_debug as it uses Alerts to correctly detect errors
--
--
--  Copyright (c) 2005 - 2016 by SynthWorks Design Inc.  All rights reserved.
--
--  Verbatim copies of this source file may be used and 
--  distributed without restriction.   
-- 								 
--  This source file may be modified and distributed under 
--  the terms of the ARTISTIC License as published by 
--  The Perl Foundation; either version 2.0 of the License, 
--  or (at your option) any later version. 						 
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
use ieee.std_logic_1164.all ; 
use ieee.numeric_std.all ; 

--library osvvm ; 
--use osvvm.AlertLogPkg.all ; 

package ResolutionPkg is 
  constant MULTIPLE_DRIVER_SEVERITY : severity_level := ERROR ; 

--
-- Note that not all simulators support resolution functions of the form:
--    subtype  std_logic_vector_max is (resolved_max) std_ulogic_vector ;
--
-- Hence, types of the form are offered as a temporary workaround until they do: 
--    std_logic_vector_max_c is array (natural range <>) of std_logic_max ; -- for non VHDL-2008
--

  -- resolved_max
  --   return maximum value.  
  --   No initializations required on ports, default of type'left is ok
  function resolved_max ( s : std_ulogic_vector) return std_ulogic ; 
  subtype  std_logic_max is resolved_max std_ulogic ; 
  subtype  std_logic_vector_max is (resolved_max) std_ulogic_vector ;
  type     std_logic_vector_max_c is array (natural range <>) of std_logic_max ; -- for non VHDL-2008  

  subtype  unsigned_max is (resolved_max) unresolved_unsigned ; 
  type     unsigned_max_c is array (natural range <>) of std_logic_max ; -- for non VHDL-2008  
  subtype  signed_max   is (resolved_max) unresolved_signed ;
  type     signed_max_c is array (natural range <>) of std_logic_max ; -- for non VHDL-2008  

  function resolved_max ( s : bit_vector) return bit ;               
  subtype  bit_max is resolved_max bit ; 
  subtype  bit_vector_max is (resolved_max) bit_vector ; 
  type     bit_vector_max_c is array (natural range <>) of bit_max ; -- for non VHDL-2008  

  function resolved_max ( s : integer_vector ) return integer ;      
  subtype  integer_max is resolved_max integer ; 
  subtype  integer_vector_max is (resolved_max) integer_vector ; 
  type     integer_vector_max_c is array (natural range <>) of integer_max ; -- for non VHDL-2008  

  function resolved_max ( s : time_vector ) return time ;            
  subtype  time_max is resolved_max time ; 
  subtype  time_vector_max is (resolved_max) time_vector ; 
  type     time_vector_max_c is array (natural range <>) of time_max ; -- for non VHDL-2008  

  function resolved_max ( s : real_vector ) return real ;            
  subtype  real_max is resolved_max real ; 
  subtype  real_vector_max is (resolved_max) real_vector ; 
  type     real_vector_max_c is array (natural range <>) of real_max ; -- for non VHDL-2008  

  function resolved_max ( s : string) return character ;             
  subtype  character_max is resolved_max character ; 
  subtype  string_max is (resolved_max) string ; 
  type     string_max_c is array (positive range <>) of character_max ; -- for non VHDL-2008  

  function resolved_max ( s : boolean_vector) return boolean ;       
  subtype  boolean_max is resolved_max boolean ; 
  subtype  boolean_vector_max is (resolved_max) boolean_vector ; 
  type     boolean_vector_max_c is array (natural range <>) of boolean_max ; -- for non VHDL-2008  

  
  -- return sum of values that /= type'left
  -- No initializations required on ports, default of type'left is ok
  function resolved_sum ( s : integer_vector ) return integer ;      
  subtype  integer_sum is resolved_sum integer ; 
  subtype  integer_vector_sum is (resolved_sum) integer_vector ; 
  type     integer_vector_sum_c is array (natural range <>) of integer_sum ; -- for non VHDL-2008  

  function resolved_sum ( s : time_vector ) return time ;            
  subtype  time_sum is resolved_sum time ; 
  subtype  time_vector_sum is (resolved_sum) time_vector ; 
  type     time_vector_sum_c is array (natural range <>) of time_sum ; -- for non VHDL-2008  

  function resolved_sum ( s : real_vector ) return real ;            
  subtype  real_sum is resolved_sum real ; 
  subtype  real_vector_sum is (resolved_sum) real_vector ; 
  type     real_vector_sum_c is array (natural range <>) of real_sum ; -- for non VHDL-2008  

  
  -- resolved_weak
  -- Special just for std_ulogic
  -- No initializations required on ports, default of type'left is ok
  function resolved_weak (s : std_ulogic_vector) return std_ulogic ;  -- no init, type'left
  subtype  std_logic_weak is resolved_weak std_ulogic ; 
  subtype  std_logic_vector_weak is (resolved_weak) std_ulogic_vector ;  


  -- legacy stuff
  -- requires ports to be initialized to 0 in the appropriate type.  
  function resolved ( s : integer_vector ) return integer ;     
  subtype  resolved_integer is resolved integer ;

  function resolved ( s : time_vector ) return time ; 
  subtype  resolved_time is resolved time ;

  function resolved ( s : real_vector ) return real ; 
  subtype  resolved_real is resolved real ;
  
  function resolved (s : string) return character ;      -- same as resolved_max
  subtype  resolved_character is resolved character ;
  -- subtype  resolved_string is (resolved) string ;  -- subtype will replace type later 
  type resolved_string is array (positive range <>) of resolved_character;  -- will change to subtype -- assert but no init

  function resolved ( s : boolean_vector) return boolean ;  --same as resolved_max  
  subtype  resolved_boolean is resolved boolean ;

end package ResolutionPkg ;
package body ResolutionPkg is 

  -- resolved_max
  -- return maximum value.  Assert FAILURE if more than 1 /= type'left
  -- No initializations required on ports, default of type'left is ok

  -- Optimized version is just the following:  
  --  ------------------------------------------------------------
  --  function resolved_max ( s : <array_type> ) return <element_type> is 
  --  ------------------------------------------------------------
  --  begin
  --    return maximum(s) ;  
  --  end function resolved_max ; 

  ------------------------------------------------------------
  function resolved_max (s : std_ulogic_vector) return std_ulogic is 
  ------------------------------------------------------------ 
  begin
    return maximum(s) ; 
  end function resolved_max ; 

  ------------------------------------------------------------
  function resolved_max ( s : bit_vector ) return bit is 
  ------------------------------------------------------------ 
  begin
    return maximum(s) ; 
  end function resolved_max ; 

  ------------------------------------------------------------
  function resolved_max ( s : integer_vector ) return integer is 
  ------------------------------------------------------------ 
  begin
    return maximum(s) ; 
  end function resolved_max ; 
  
  ------------------------------------------------------------
  function resolved_max ( s : time_vector ) return time is 
  ------------------------------------------------------------
  begin
    return maximum(s) ; 
  end function resolved_max ; 

  ------------------------------------------------------------
  function resolved_max ( s : real_vector ) return real is 
  ------------------------------------------------------------
  begin
    return maximum(s) ; 
  end function resolved_max ; 
  
  ------------------------------------------------------------
  function resolved_max ( s : string ) return character is 
  ------------------------------------------------------------
  begin
    return maximum(s) ; 
  end function resolved_max ; 

  ------------------------------------------------------------
  function resolved_max ( s : boolean_vector) return boolean is
  ------------------------------------------------------------
  begin
    return maximum(s) ; 
  end function resolved_max ; 

  
  -- resolved_sum - appropriate for numeric types
  -- return sum of values that /= type'left
  -- No initializations required on ports, default of type'left is ok
  ------------------------------------------------------------
  function resolved_sum ( s : integer_vector ) return integer is 
  ------------------------------------------------------------
    variable result : integer := 0 ; 
  begin
    for i in s'RANGE loop
      if s(i) /= integer'left then 
        result := s(i) + result;
      end if ;
    end loop ;
    return result ; 
  end function resolved_sum ; 
  
  ------------------------------------------------------------
  function resolved_sum ( s : time_vector ) return time is 
  ------------------------------------------------------------
    variable result : time := 0 sec ; 
  begin
    for i in s'RANGE loop
      if s(i) /= time'left then 
        result := s(i) + result;
      end if ;
    end loop ;
    return result ; 
  end function resolved_sum ; 

  ------------------------------------------------------------
  function resolved_sum ( s : real_vector ) return real is 
  ------------------------------------------------------------
    variable result : real := 0.0 ; 
  begin
    for i in s'RANGE loop
      if s(i) /= real'left then 
        result := s(i) + result;
      end if ;
    end loop ;
    return result ; 
  end function resolved_sum ; 
  
  
  -- resolved_weak
  -- Special just for std_ulogic
  -- No initializations required on ports, default of type'left is ok
  type stdlogic_table is array(STD_ULOGIC, STD_ULOGIC) of STD_ULOGIC;

  constant weak_resolution_table : stdlogic_table := (
    --  Resolution order:  Z < U < W < X < - < L < H < 0 < 1
    --      ---------------------------------------------------------
    --      |  U    X    0    1    Z    W    L    H    -        |   |  
    --      ---------------------------------------------------------
             ('U', 'X', '0', '1', 'U', 'W', 'L', 'H', '-'),  -- | U |
             ('X', 'X', '0', '1', 'X', 'X', 'L', 'H', '-'),  -- | X |
             ('0', '0', '0', '1', '0', '0', '0', '0', '0'),  -- | 0 |
             ('1', '1', '1', '1', '1', '1', '1', '1', '1'),  -- | 1 |
             ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-'),  -- | Z |
             ('W', 'X', '0', '1', 'W', 'W', 'L', 'H', '-'),  -- | W |
             ('L', 'L', '0', '1', 'L', 'L', 'L', 'H', 'L'),  -- | L |
             ('H', 'H', '0', '1', 'H', 'H', 'W', 'H', 'H'),  -- | H |
             ('-', '-', '0', '1', '-', '-', 'L', 'H', '-')   -- | - |
             );
             
  ------------------------------------------------------------
  function resolved_weak (s : std_ulogic_vector) return std_ulogic is
  ------------------------------------------------------------
    variable result : std_ulogic := 'Z' ; 
  begin
    for i in s'RANGE loop
      result := weak_resolution_table(result, s(i)) ; 
    end loop ;
    return result ; 
  end function resolved_weak ; 

  
  -- legacy stuff.
  -- requires ports to be initialized to 0 in the appropriate type.  
  
  ------------------------------------------------------------
  function resolved ( s : integer_vector ) return integer is 
  -- requires interface to be initialized to 0
  ------------------------------------------------------------
    variable result : integer := 0 ; 
    variable failed : boolean := FALSE ; 
  begin
    for i in s'RANGE loop
      if s(i) /= 0 then 
        failed := failed or (result /= 0) ;
        result := maximum(s(i),result);
      end if ;
    end loop ;
    assert not failed report "ResolutionPkg.resolved: multiple drivers on integer" severity MULTIPLE_DRIVER_SEVERITY ; 
    -- AlertIf(OSVVM_ALERTLOG_ID, failed, "ResolutionPkg.resolved: multiple drivers on integer") ; 
    return result ; 
  end function resolved ; 

  ------------------------------------------------------------
  function resolved ( s : time_vector ) return time is 
  -- requires interface to be initialized to 0 ns
  ------------------------------------------------------------
    variable result : time := 0 ns ; 
    variable failed : boolean := FALSE ; 
  begin
    for i in s'RANGE loop
      if s(i) > 0 ns then 
        failed := failed or (result /= 0 ns) ;
        result := maximum(s(i),result);
      end if ;
    end loop ;
    assert not failed report "ResolutionPkg.resolved: multiple drivers on time" severity MULTIPLE_DRIVER_SEVERITY ; 
    -- AlertIf(OSVVM_ALERTLOG_ID, failed, "ResolutionPkg.resolved: multiple drivers on time") ; 
    return result ; 
  end function resolved ; 

  ------------------------------------------------------------
  function resolved ( s : real_vector ) return real is 
  -- requires interface to be initialized to 0.0
  ------------------------------------------------------------
    variable result : real := 0.0 ; 
    variable failed : boolean := FALSE ; 
  begin
    for i in s'RANGE loop
      if s(i) /= 0.0 then 
        failed := failed or (result /= 0.0) ;
        result := maximum(s(i),result);
      end if ;
    end loop ;
    assert not failed report "ResolutionPkg.resolved: multiple drivers on real" severity MULTIPLE_DRIVER_SEVERITY ; 
    -- AlertIf(OSVVM_ALERTLOG_ID, failed, "ResolutionPkg.resolved: multiple drivers on real") ; 
    return result ; 
  end function resolved ; 

  ------------------------------------------------------------
  function resolved (s : string) return character is  
  -- same as resolved_max
  ------------------------------------------------------------
    variable result : character := NUL ; 
    variable failed : boolean := FALSE ; 
  begin
    for i in s'RANGE loop
      if s(i) /= NUL then 
        failed := failed or (result /= NUL) ;
        result := maximum(result, s(i)) ; 
      end if ; 
    end loop ;
    assert not failed report "ResolutionPkg.resolved: multiple drivers on character" severity MULTIPLE_DRIVER_SEVERITY ; 
    -- AlertIf(OSVVM_ALERTLOG_ID, failed, "ResolutionPkg.resolved: multiple drivers on character") ; 
    return result ; 
  end function resolved ; 

  ------------------------------------------------------------
  function resolved ( s : boolean_vector) return boolean is
  -- same as resolved_max
  ------------------------------------------------------------
    variable result : boolean := FALSE ; 
    variable failed : boolean := FALSE ; 
  begin
    for i in s'RANGE loop
      if s(i) then 
        failed := failed or result ; 
        result := TRUE ;
      end if ; 
    end loop ;
    assert not failed report "ResolutionPkg.resolved: multiple drivers on boolean" severity MULTIPLE_DRIVER_SEVERITY ; 
    -- AlertIf(OSVVM_ALERTLOG_ID, failed, "ResolutionPkg.resolved: multiple drivers on boolean") ; 
    return result ; 
  end function resolved ; 

end package body ResolutionPkg ;
