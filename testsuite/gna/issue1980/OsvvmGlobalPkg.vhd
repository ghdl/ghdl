--
--  File Name:         OsvvmGlobalPkg.vhd
--  Design Unit Name:  OsvvmGlobalPkg
--  Revision:          STANDARD VERSION,  revision 2015.01
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis      jim@synthworks.com
--
--
--  Description:
--        Global Settings for OSVVM packages
--
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--  Revision History:
--    Date      Version    Description
--    02/2022   2022.02    Added support for IdSeparator.  
--                         Supports PrintParent mode PRINT_NAME_AND_PARENT.  <Parent Name> <IdSeparator> <AlertLogID Name>.   
--    01/2020   2020.01    Updated Licenses to Apache
--    01/2014   2015.01    Initial revision
--
--
--  This file is part of OSVVM.
--  
--  Copyright (c) 2015 - 2020 by SynthWorks Design Inc.  
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

library ieee ;
use std.textio.all ;

use work.NamePkg.all ; 

package OsvvmGlobalPkg is
  -- FILE IO Global File Identifier -- Open using AlertLogPkg.TranscriptOpen
--  file     TranscriptFile : text ;

  -- Shared Options Type used in OSVVM
  type OsvvmOptionsType is (OPT_INIT_PARM_DETECT, OPT_USE_DEFAULT, DISABLED, FALSE, ENABLED, TRUE) ;
  function IsEnabled (A : OsvvmOptionsType) return boolean ;  -- Requires that TRUE is last and ENABLED is 2nd to last
  function to_OsvvmOptionsType (A : boolean) return OsvvmOptionsType ;

  -- Defaults for String values
  constant OSVVM_DEFAULT_ALERT_PREFIX     : string := "%% Alert" ;
  constant OSVVM_DEFAULT_LOG_PREFIX       : string := "%% Log  " ;
  constant OSVVM_DEFAULT_WRITE_PREFIX     : string := "%% " ;
  constant OSVVM_DEFAULT_DONE_NAME        : string := "DONE" ;
  constant OSVVM_DEFAULT_PASS_NAME        : string := "PASSED" ;
  constant OSVVM_DEFAULT_FAIL_NAME        : string := "FAILED" ;
  constant OSVVM_DEFAULT_ID_SEPARATOR     : string := ": " ; 
  constant OSVVM_STRING_INIT_PARM_DETECT  : string := NUL & NUL & NUL ; 
  constant OSVVM_STRING_USE_DEFAULT       : string := NUL & "" ; 

  -- Coverage Settings
  constant OSVVM_DEFAULT_WRITE_PASS_FAIL   : OsvvmOptionsType := FALSE ;
  constant OSVVM_DEFAULT_WRITE_BIN_INFO    : OsvvmOptionsType := TRUE ;
  constant OSVVM_DEFAULT_WRITE_COUNT       : OsvvmOptionsType := TRUE ;
  constant OSVVM_DEFAULT_WRITE_ANY_ILLEGAL : OsvvmOptionsType := FALSE ;

  ------------------------------------------------------------
  procedure SetOsvvmGlobalOptions (
  ------------------------------------------------------------
    WritePassFail   : OsvvmOptionsType := OPT_INIT_PARM_DETECT ;
    WriteBinInfo    : OsvvmOptionsType := OPT_INIT_PARM_DETECT ;
    WriteCount      : OsvvmOptionsType := OPT_INIT_PARM_DETECT ;
    WriteAnyIllegal : OsvvmOptionsType := OPT_INIT_PARM_DETECT ;
    WritePrefix     : string := OSVVM_STRING_INIT_PARM_DETECT ;
    DoneName        : string := OSVVM_STRING_INIT_PARM_DETECT ;
    PassName        : string := OSVVM_STRING_INIT_PARM_DETECT ;
    FailName        : string := OSVVM_STRING_INIT_PARM_DETECT ;
    IdSeparator     : string := OSVVM_STRING_INIT_PARM_DETECT
  ) ;
  
  ------------------------------------------------------------
  -- Accessor Functions
  function ResolveOsvvmOption(A, B, C : OsvvmOptionsType) return OsvvmOptionsType ; 
  function ResolveOsvvmOption(A, B, C, D : OsvvmOptionsType) return OsvvmOptionsType ; 
  function IsOsvvmStringSet (A : string) return boolean ;
  function ResolveOsvvmOption(A, B : string) return string ; 
  function ResolveOsvvmOption(A, B, C : string) return string ; 
  function ResolveOsvvmOption(A, B, C, D : string) return string ; 
  
  impure function ResolveOsvvmWritePrefix  (A : String)  return string ; 
  impure function ResolveOsvvmDoneName     (A : String)  return string ; 
  impure function ResolveOsvvmPassName     (A : String)  return string ; 
  impure function ResolveOsvvmFailName     (A : String)  return string ; 
  impure function ResolveOsvvmIdSeparator  (A : String)  return string ; 
  
  impure function ResolveCovWritePassFail  (A : OsvvmOptionsType) return OsvvmOptionsType ;  -- Cov
  impure function ResolveCovWriteBinInfo   (A : OsvvmOptionsType) return OsvvmOptionsType ; -- Cov
  impure function ResolveCovWriteCount     (A : OsvvmOptionsType) return OsvvmOptionsType ; -- Cov
  impure function ResolveCovWriteAnyIllegal(A : OsvvmOptionsType) return OsvvmOptionsType ;  -- Cov
  
  impure function ResolveOsvvmWritePrefix  (A, B : String)  return string ; 
  impure function ResolveOsvvmDoneName     (A, B : String)  return string ; 
  impure function ResolveOsvvmPassName     (A, B : String)  return string ; 
  impure function ResolveOsvvmFailName     (A, B : String)  return string ; 

  impure function ResolveCovWritePassFail  (A, B : OsvvmOptionsType) return OsvvmOptionsType ;  -- Cov
  impure function ResolveCovWriteBinInfo   (A, B : OsvvmOptionsType) return OsvvmOptionsType ; -- Cov
  impure function ResolveCovWriteCount     (A, B : OsvvmOptionsType) return OsvvmOptionsType ; -- Cov
  impure function ResolveCovWriteAnyIllegal(A, B : OsvvmOptionsType) return OsvvmOptionsType ;  -- Cov
  
  procedure SetOsvvmDefaultTimeUnits (A : time) ; 
  impure function GetOsvvmDefaultTimeUnits return time ;

  procedure OsvvmDeallocate ;
  
  type OptionsPType is protected 
    procedure Set (A: OsvvmOptionsType) ; 
    impure function get return OsvvmOptionsType ;
  end protected OptionsPType ;

  type OsvvmDefaultTimeUnitsPType is protected 
    procedure Set (A: time) ; 
    impure function get return time ;
  end protected OsvvmDefaultTimeUnitsPType ;
end OsvvmGlobalPkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body OsvvmGlobalPkg is
  type OptionsPType is protected body
    variable GlobalVar : OsvvmOptionsType ; 
    procedure Set (A : OsvvmOptionsType) is
    begin
       GlobalVar := A ; 
    end procedure Set ; 
    impure function get return OsvvmOptionsType is
    begin
      return GlobalVar ; 
    end function get ; 
  end protected body OptionsPType ; 

  type OsvvmDefaultTimeUnitsPType is protected body
    variable GlobalVar : time := std.env.resolution_limit ;  -- VHDL-2008
    procedure Set (A : time) is
    begin
      if A > std.env.resolution_limit then
        GlobalVar := A ; 
      elsif A < std.env.resolution_limit then 
        report "SetOsvvmDefaultTimeUnits:  time unit parameter too small" severity warning ;
      end if ; 
    end procedure Set ; 
    impure function get return time is
    begin
      return GlobalVar ; 
    end function get ; 
  end protected body OsvvmDefaultTimeUnitsPType ; 
  
  shared variable WritePrefixVar           : NamePType ;
  shared variable DoneNameVar              : NamePType ;
  shared variable PassNameVar              : NamePType ;
  shared variable FailNameVar              : NamePType ;
  shared variable IdSeparatorVar           : NamePType ;
  shared variable WritePassFailVar         : OptionsPType ; -- := FALSE ;
  shared variable WriteBinInfoVar          : OptionsPType ; -- := TRUE ;
  shared variable WriteCountVar            : OptionsPType ; -- := TRUE ;
  shared variable WriteAnyIllegalVar       : OptionsPType ; -- := FALSE ;
  shared variable OsvvmDefaultTimeUnitsVar : OsvvmDefaultTimeUnitsPType ; 

  function IsEnabled (A : OsvvmOptionsType) return boolean is
  begin
    return A >= ENABLED ; 
  end function IsEnabled ; 
  
  function to_OsvvmOptionsType (A : boolean) return OsvvmOptionsType is
  begin
    if A then 
      return TRUE ; 
    else 
      return FALSE ;
    end if ; 
  end function to_OsvvmOptionsType ; 
  

  ------------------------------------------------------------
  procedure SetOsvvmGlobalOptions (
  ------------------------------------------------------------
    WritePassFail   : OsvvmOptionsType := OPT_INIT_PARM_DETECT ;
    WriteBinInfo    : OsvvmOptionsType := OPT_INIT_PARM_DETECT ;
    WriteCount      : OsvvmOptionsType := OPT_INIT_PARM_DETECT ;
    WriteAnyIllegal : OsvvmOptionsType := OPT_INIT_PARM_DETECT ;
    WritePrefix     : string := OSVVM_STRING_INIT_PARM_DETECT ;
    DoneName        : string := OSVVM_STRING_INIT_PARM_DETECT ;
    PassName        : string := OSVVM_STRING_INIT_PARM_DETECT ;
    FailName        : string := OSVVM_STRING_INIT_PARM_DETECT ;
    IdSeparator     : string := OSVVM_STRING_INIT_PARM_DETECT
  ) is
  begin
    if WritePassFail /= OPT_INIT_PARM_DETECT then
      WritePassFailVar.Set(WritePassFail) ;
    end if ;
    if WriteBinInfo /= OPT_INIT_PARM_DETECT then
      WriteBinInfoVar.Set(WriteBinInfo) ;
    end if ;
    if WriteCount /= OPT_INIT_PARM_DETECT then
      WriteCountVar.Set(WriteCount) ;
    end if ;
    if WriteAnyIllegal /= OPT_INIT_PARM_DETECT then
      WriteAnyIllegalVar.Set(WriteAnyIllegal) ;
    end if ;
    if WritePrefix /= OSVVM_STRING_INIT_PARM_DETECT then
      WritePrefixVar.Set(WritePrefix) ; 
    end if ;
    if DoneName /= OSVVM_STRING_INIT_PARM_DETECT then
      DoneNameVar.Set(DoneName) ; 
    end if ;
    if PassName /= OSVVM_STRING_INIT_PARM_DETECT then
      PassNameVar.Set(PassName) ; 
    end if ;
    if FailName /= OSVVM_STRING_INIT_PARM_DETECT then
      FailNameVar.Set(FailName) ; 
    end if ;
    if IdSeparator /= OSVVM_STRING_INIT_PARM_DETECT then
      IdSeparatorVar.Set(IdSeparator) ; 
    end if ;
  end procedure SetOsvvmGlobalOptions ;  

  ------------------------------------------------------------
  -- Accessor Functions
  -- Local Function
  function IsOsvvmOptionSet (A : OsvvmOptionsType) return boolean is
  begin
    return A > OPT_USE_DEFAULT ; 
  end function IsOsvvmOptionSet ;

  function ResolveOsvvmOption(A, B, C : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    if IsOsvvmOptionSet(A) then
      return A ;
    elsif IsOsvvmOptionSet(B) then
      return B ;
    else
      return C ;
    end if ;
  end function ResolveOsvvmOption ;
 
  function ResolveOsvvmOption(A, B, C, D : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    if IsOsvvmOptionSet(A) then
      return A ;
    elsif IsOsvvmOptionSet(B) then
      return B ;
    elsif IsOsvvmOptionSet(C) then
      return C ;
    else
      return D ;
    end if ;
  end function ResolveOsvvmOption ;
  
  -- Local Function
  function IsOsvvmStringSet (A : string) return boolean is
  begin
    if A'length = 0 then   -- Null strings permitted
      return TRUE ;     
    else 
      return A(A'left) /= NUL ;
    end if; 
  end function IsOsvvmStringSet ;
  
  function ResolveOsvvmOption(A, B : string) return string is
  begin
    if IsOsvvmStringSet(A) then
      return A ;
    else
      return B ;
    end if ;
  end function ResolveOsvvmOption ; 
  
  function ResolveOsvvmOption(A, B, C : string) return string is
  begin
    if IsOsvvmStringSet(A) then
      return A ;
    elsif IsOsvvmStringSet(B) then
      return B ;
    else
      return C ;
    end if ;
  end function ResolveOsvvmOption ; 
  
  function ResolveOsvvmOption(A, B, C, D : string) return string is
  begin
    if IsOsvvmStringSet(A) then
      return A ;
    elsif IsOsvvmStringSet(B) then
      return B ;
    elsif IsOsvvmStringSet(C) then
      return C ;
    else
      return D ;
    end if ;
  end function ResolveOsvvmOption ;
  

  impure function ResolveOsvvmWritePrefix(A : String)  return string is
  begin
    return ResolveOsvvmOption(A, WritePrefixVar.GetOpt, OSVVM_DEFAULT_WRITE_PREFIX) ;
  end function ResolveOsvvmWritePrefix ; 
  
  impure function ResolveOsvvmDoneName(A : String)  return string is
  begin
    return ResolveOsvvmOption(A, DoneNameVar.GetOpt, OSVVM_DEFAULT_DONE_NAME) ;
  end function ResolveOsvvmDoneName ; 
  
  impure function ResolveOsvvmPassName(A : String)  return string is
  begin
    return ResolveOsvvmOption(A, PassNameVar.GetOpt, OSVVM_DEFAULT_PASS_NAME) ;
  end function ResolveOsvvmPassName ; 
  
  impure function ResolveOsvvmFailName(A : String)  return string is
  begin
    return ResolveOsvvmOption(A, FailNameVar.GetOpt, OSVVM_DEFAULT_FAIL_NAME) ;
  end function ResolveOsvvmFailName ;  
  
  impure function ResolveOsvvmIdSeparator(A : String)  return string is
  begin
    return ResolveOsvvmOption(A, IdSeparatorVar.GetOpt, OSVVM_DEFAULT_ID_SEPARATOR) ;
  end function ResolveOsvvmIdSeparator ;  
  
 
  impure function ResolveCovWritePassFail(A : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    return ResolveOsvvmOption(A, WritePassFailVar.Get, OSVVM_DEFAULT_WRITE_PASS_FAIL) ;
  end function ResolveCovWritePassFail ;  -- Cov
  
  impure function ResolveCovWriteBinInfo(A : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    return ResolveOsvvmOption(A, WriteBinInfoVar.Get, OSVVM_DEFAULT_WRITE_BIN_INFO) ;
  end function ResolveCovWriteBinInfo ;  -- Cov

  impure function ResolveCovWriteCount(A : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    return ResolveOsvvmOption(A, WriteCountVar.Get, OSVVM_DEFAULT_WRITE_COUNT) ;
  end function ResolveCovWriteCount ;  -- Cov

  impure function ResolveCovWriteAnyIllegal(A : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    return ResolveOsvvmOption(A, WriteAnyIllegalVar.Get, OSVVM_DEFAULT_WRITE_ANY_ILLEGAL) ;
  end function ResolveCovWriteAnyIllegal ;  -- Cov
  

  impure function ResolveOsvvmWritePrefix(A, B : String)  return string is
  begin
    return ResolveOsvvmOption(A, B, WritePrefixVar.GetOpt, OSVVM_DEFAULT_WRITE_PREFIX) ;
  end function ResolveOsvvmWritePrefix ; 
  
  impure function ResolveOsvvmDoneName(A, B : String)  return string is
  begin
    return ResolveOsvvmOption(A, DoneNameVar.GetOpt, OSVVM_DEFAULT_DONE_NAME) ;
  end function ResolveOsvvmDoneName ; 
  
  impure function ResolveOsvvmPassName(A, B : String)  return string is
  begin
    return ResolveOsvvmOption(A, B, PassNameVar.GetOpt, OSVVM_DEFAULT_PASS_NAME) ;
  end function ResolveOsvvmPassName ; 

  impure function ResolveOsvvmFailName(A, B : String)  return string is
  begin
    return ResolveOsvvmOption(A, B, FailNameVar.GetOpt, OSVVM_DEFAULT_FAIL_NAME) ;
  end function ResolveOsvvmFailName ;  


  impure function ResolveCovWritePassFail(A, B : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    return ResolveOsvvmOption(A, B, WritePassFailVar.Get, OSVVM_DEFAULT_WRITE_PASS_FAIL) ;
  end function ResolveCovWritePassFail ;  -- Cov
  
  impure function ResolveCovWriteBinInfo(A, B : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    return ResolveOsvvmOption(A, B, WriteBinInfoVar.Get, OSVVM_DEFAULT_WRITE_BIN_INFO) ;
  end function ResolveCovWriteBinInfo ;  -- Cov

  impure function ResolveCovWriteCount(A, B : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    return ResolveOsvvmOption(A, B, WriteCountVar.Get, OSVVM_DEFAULT_WRITE_COUNT) ;
  end function ResolveCovWriteCount ;  -- Cov

  impure function ResolveCovWriteAnyIllegal(A, B : OsvvmOptionsType) return OsvvmOptionsType is
  begin
    return ResolveOsvvmOption(A, B, WriteAnyIllegalVar.Get, OSVVM_DEFAULT_WRITE_ANY_ILLEGAL) ;
  end function ResolveCovWriteAnyIllegal ;  -- Cov
  
  procedure SetOsvvmDefaultTimeUnits (A : time) is 
  begin
    OsvvmDefaultTimeUnitsVar.Set(A) ; 
  end procedure SetOsvvmDefaultTimeUnits ; 

  impure function GetOsvvmDefaultTimeUnits return time is 
  begin
    return OsvvmDefaultTimeUnitsVar.Get ; 
  end function GetOsvvmDefaultTimeUnits ; 
  
  procedure OsvvmDeallocate is
  begin
    -- Free up space used by NamePType within OsvvmGlobalPkg
    WritePrefixVar.Deallocate ;
    DoneNameVar.Deallocate ;
    PassNameVar.Deallocate ;
    FailNameVar.Deallocate ;
    WritePassFailVar.Set(FALSE) ; -- := FALSE ;
    WriteBinInfoVar.Set(TRUE ) ; -- := TRUE  ;
    WriteCountVar.Set(TRUE ) ; -- := TRUE  ;
    WriteAnyIllegalVar.Set(FALSE) ; -- := FALSE ;

  end procedure OsvvmDeallocate ; 
   
end package body OsvvmGlobalPkg ;