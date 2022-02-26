--
--  File Name:         NamePkg.vhd
--  Design Unit Name:  NamePkg
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis          SynthWorks
--
--
--  Package Defines
--      Data structure for name. 
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--  Revision History:
--    Date      Version    Description
--    02/2022   2022.02    Added NameLength method to NamePType
--    01/2020   2020.01    Updated Licenses to Apache
--    05/2015   2015.06    Added input to Get to return when not initialized
--    12/2014:  2014.07a   Removed initialized pointers which can lead to memory leaks.
--    07/2014:  2014.07    Moved specialization required by CoveragePkg to CoveragePkg
--                         Separated name handling from message handling to simplify naming
--    06/2010:  0.1        Initial revision
--
--
--  This file is part of OSVVM.
--  
--  Copyright (c) 2010 - 2020 by SynthWorks Design Inc.  
--  
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--  
--      https://www.apache.org/licenses/LICENSE-2.0
--  
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--  

use std.textio.all ;

package NamePkg is

  type NamePType is protected
    procedure Set (NameIn : String) ;
    impure function Get (DefaultName : string := "") return string ;
    impure function GetOpt return string ;
    impure function IsSet return boolean ; 
    impure function NameLength return integer ; 
    procedure Clear ; -- clear name
    procedure Deallocate ; -- effectively alias to clear name
  end protected NamePType ;

end package NamePkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body NamePkg is
  type NamePType is protected body
  
    variable NamePtr   : line ;

    ------------------------------------------------------------
    procedure Set (NameIn : String) is
    ------------------------------------------------------------
    begin
      deallocate(NamePtr) ;
      NamePtr := new string'(NameIn) ;
    end procedure Set ;

    ------------------------------------------------------------
    impure function Get (DefaultName : string := "") return string is
    ------------------------------------------------------------
    begin
      if NamePtr = NULL then 
        return DefaultName ; 
      else
        return NamePtr.all ; 
      end if ; 
    end function Get ;

    ------------------------------------------------------------
    impure function GetOpt return string is
    ------------------------------------------------------------
    begin
      if NamePtr = NULL then 
        return NUL & "" ; 
      else
        return NamePtr.all ; 
      end if ; 
    end function GetOpt ;

    ------------------------------------------------------------
    impure function IsSet return boolean is 
    ------------------------------------------------------------
    begin
      return NamePtr /= NULL ; 
    end function IsSet ;      
    
    ------------------------------------------------------------
    impure function NameLength return integer is
    ------------------------------------------------------------
    begin
      if NamePtr = NULL then 
        return 0 ; 
      else
        return NamePtr.all'length ; 
      end if ; 
    end function NameLength ;

    ------------------------------------------------------------
    procedure Clear is  -- clear name
    ------------------------------------------------------------
    begin
      deallocate(NamePtr) ;
    end procedure Clear ;
    
    ------------------------------------------------------------
    procedure Deallocate is  -- clear name
    ------------------------------------------------------------
    begin
      Clear ; 
    end procedure Deallocate ;

  end protected body NamePType ;

end package body NamePkg ;