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
--  Latest standard version available at:
--        http://www.SynthWorks.com/downloads
--
--  Revision History:
--    Date      Version    Description
--    06/2010:  0.1        Initial revision
--    07/2014:  2014.07    Moved specialization required by CoveragePkg to CoveragePkg
--                         Separated name handling from message handling to simplify naming
--    12/2014:  2014.07a   Removed initialized pointers which can lead to memory leaks.
--    05/2015   2015.06    Added input to Get to return when not initialized
--
--
--  Copyright (c) 2010 - 2015 by SynthWorks Design Inc.  All rights reserved.
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

use std.textio.all ;

package NamePkg is

  type NamePType is protected
    procedure Set (NameIn : String) ;
    impure function Get (DefaultName : string := "") return string ;
    impure function GetOpt return string ;
    impure function IsSet return boolean ; 
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