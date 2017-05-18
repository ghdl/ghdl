--
--  File Name:         AlertLogPkg.vhd
--  Design Unit Name:  AlertLogPkg
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis      jim@synthworks.com
--
--
--  Description:
--        Alert handling and log filtering (verbosity control)
--        Alert handling provides a method to count failures, errors, and warnings
--          To accumlate counts, a data structure is created in a shared variable
--          It is of type AlertLogStructPType which is defined in AlertLogBasePkg
--        Log filtering provides verbosity control for logs (display or do not display)
--        AlertLogPkg provides a simplified interface to the shared variable 
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
--    01/2015:   2015.01    Initial revision
--    03/2015    2015.03    Added:  AlertIfEqual, AlertIfNotEqual, AlertIfDiff, PathTail, 
--                          ReportNonZeroAlerts, ReadLogEnables 
--    05/2015    2015.06    Added IncAlertCount, AffirmIf
--    07/2015    2016.01    Fixed AlertLogID issue with > 32 IDs
--    02/2016    2016.02    Fixed IsLogEnableType (for PASSED), AffirmIf (to pass AlertLevel)
--                          Created LocalInitialize
--
--  Copyright (c) 2015 - 2016 by SynthWorks Design Inc.  All rights reserved.
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
use work.OsvvmGlobalPkg.all ; 
use work.TranscriptPkg.all ;
use work.TextUtilPkg.all ;

library IEEE ; 
use ieee.std_logic_1164.all ; 
use ieee.numeric_std.all ; 

package AlertLogPkg is

  subtype  AlertLogIDType   is integer ;
  type     AlertType        is (FAILURE, ERROR, WARNING) ;  -- NEVER
  subtype  AlertIndexType   is AlertType range FAILURE to WARNING ;
  type     AlertCountType   is array (AlertIndexType) of integer ;
  type     AlertEnableType  is array(AlertIndexType) of boolean ;
  type     LogType          is (ALWAYS, DEBUG, FINAL, INFO, PASSED) ;  -- NEVER  -- See function IsLogEnableType
  subtype  LogIndexType     is LogType range DEBUG to PASSED ;
  type     LogEnableType    is array (LogIndexType) of boolean ;

  constant  ALERTLOG_BASE_ID               : AlertLogIDType := 0 ;  -- Careful as some code may assume this is 0.  
  constant  ALERTLOG_DEFAULT_ID            : AlertLogIDType := 1 ; 
  constant  ALERT_DEFAULT_ID               : AlertLogIDType := ALERTLOG_DEFAULT_ID ;
  constant  LOG_DEFAULT_ID                 : AlertLogIDType := ALERTLOG_DEFAULT_ID ;
  constant  OSVVM_ALERTLOG_ID              : AlertLogIDType := 2 ;
  constant  OSVVM_SCOREBOARD_ALERTLOG_ID   : AlertLogIDType := OSVVM_ALERTLOG_ID ;
  -- NUM_PREDEFINED_AL_IDS intended to be local, but depends on others
  -- constant  NUM_PREDEFINED_AL_IDS          : AlertLogIDType := OSVVM_SCOREBOARD_ALERTLOG_ID - ALERTLOG_BASE_ID ;  -- Not including base
  constant  ALERTLOG_ID_NOT_FOUND          : AlertLogIDType := -1 ;  -- alternately integer'right
  constant  ALERTLOG_ID_NOT_ASSIGNED       : AlertLogIDType := -1 ;
  constant  MIN_NUM_AL_IDS                 : AlertLogIDType := 32 ; -- Number IDs initially allocated

  alias AlertLogOptionsType is work.OsvvmGlobalPkg.OsvvmOptionsType ; 

  ------------------------------------------------------------
  --  Alert always goes to the transcript file
  procedure Alert( 
    AlertLogID   : AlertLogIDType ; 
    Message      : string ;
    Level        : AlertType := ERROR  
  ) ;
  procedure Alert( Message : string ; Level : AlertType := ERROR ) ; 

  ------------------------------------------------------------
  procedure IncAlertCount(   -- A silent form of alert
    AlertLogID   : AlertLogIDType ; 
    Level        : AlertType := ERROR
  ) ; 
  procedure IncAlertCount( Level : AlertType := ERROR ) ; 

  ------------------------------------------------------------
  -- Similar to assert, except condition is positive
  procedure AlertIf( AlertLogID : AlertLogIDType ; condition : boolean ; Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIf( condition : boolean ; Message : string ; Level : AlertType := ERROR ) ; 
  impure function  AlertIf( AlertLogID : AlertLogIDType ; condition : boolean ; Message : string ; Level : AlertType := ERROR ) return boolean ; 
  impure function  AlertIf( condition : boolean ; Message : string ; Level : AlertType := ERROR ) return boolean ; 

  -- deprecated
  procedure AlertIf( condition : boolean ; AlertLogID : AlertLogIDType ; Message : string ; Level : AlertType := ERROR )  ; 
  impure function  AlertIf( condition : boolean ; AlertLogID : AlertLogIDType ; Message : string ; Level : AlertType := ERROR ) return boolean ; 

  ------------------------------------------------------------
  -- Direct replacement for assert
  procedure AlertIfNot( AlertLogID : AlertLogIDType ; condition : boolean ; Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNot( condition : boolean ; Message : string ; Level : AlertType := ERROR ) ; 
  impure function  AlertIfNot( AlertLogID : AlertLogIDType ; condition : boolean ; Message : string ; Level : AlertType := ERROR ) return boolean ; 
  impure function  AlertIfNot( condition : boolean ; Message : string ; Level : AlertType := ERROR ) return boolean ; 

  -- deprecated
  procedure AlertIfNot( condition : boolean ; AlertLogID : AlertLogIDType ; Message : string ; Level : AlertType := ERROR )  ; 
  impure function  AlertIfNot( condition : boolean ; AlertLogID : AlertLogIDType ; Message : string ; Level : AlertType := ERROR ) return boolean ; 
  
  ------------------------------------------------------------
  -- overloading for common functionality
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ;  L, R : std_logic ;         Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ;  L, R : std_logic_vector ;  Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ;  L, R : unsigned ;          Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ;  L, R : signed ;            Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ;  L, R : integer ;           Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ;  L, R : real ;              Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ;  L, R : character ;         Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ;  L, R : string ;            Message : string ; Level : AlertType := ERROR )  ; 

  procedure AlertIfEqual( L, R : std_logic ;        Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( L, R : std_logic_vector ; Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( L, R : unsigned ;         Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( L, R : signed ;           Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( L, R : integer ;          Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( L, R : real ;             Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( L, R : character ;        Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfEqual( L, R : string ;           Message : string ; Level : AlertType := ERROR )  ; 

  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ;  L, R : std_logic ;        Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ;  L, R : std_logic_vector ; Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ;  L, R : unsigned ;         Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ;  L, R : signed ;           Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ;  L, R : integer ;          Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ;  L, R : real ;             Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ;  L, R : character ;        Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ;  L, R : string ;           Message : string ; Level : AlertType := ERROR )  ; 
  
  procedure AlertIfNotEqual( L, R : std_logic ;        Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( L, R : std_logic_vector ; Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( L, R : unsigned ;         Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( L, R : signed ;           Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( L, R : integer ;          Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( L, R : real ;             Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( L, R : character ;        Message : string ; Level : AlertType := ERROR )  ; 
  procedure AlertIfNotEqual( L, R : string ;           Message : string ; Level : AlertType := ERROR )  ; 
  ------------------------------------------------------------
  -- Simple Diff for file comparisons
  procedure AlertIfDiff (AlertLogID : AlertLogIDType ; Name1, Name2 : string; Message : string := "" ; Level : AlertType := ERROR ) ; 
  procedure AlertIfDiff (Name1, Name2 : string; Message : string := "" ; Level : AlertType := ERROR ) ; 
  procedure AlertIfDiff (AlertLogID : AlertLogIDType ; file File1, File2 : text; Message : string := "" ; Level : AlertType := ERROR ) ; 
  procedure AlertIfDiff (file File1, File2 : text; Message : string := "" ; Level : AlertType := ERROR ) ; 
  ------------------------------------------------------------
  procedure AffirmIf(
    AlertLogID   : AlertLogIDType ;
    condition    : boolean ;
    Message      : string ;
    LogLevel     : LogType := PASSED ;
    AlertLevel   : AlertType := ERROR
  ) ;
  procedure AffirmIf(condition : boolean ; Message : string ;  LogLevel : LogType := PASSED ; AlertLevel : AlertType := ERROR) ;

  ------------------------------------------------------------
  procedure SetAlertLogJustify ;
  procedure ReportAlerts ( Name : String ; AlertCount : AlertCountType ) ;
  procedure ReportAlerts ( Name : string := OSVVM_STRING_INIT_PARM_DETECT ; AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID ; ExternalErrors : AlertCountType := (others => 0) ) ;
  procedure ReportNonZeroAlerts ( Name : string := OSVVM_STRING_INIT_PARM_DETECT ; AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID ; ExternalErrors : AlertCountType := (others => 0) ) ;
  procedure ClearAlerts ; 
  function "ABS" (L : AlertCountType) return AlertCountType ;
  function "+" (L, R : AlertCountType) return AlertCountType ;
  function "-" (L, R : AlertCountType) return AlertCountType ;
  function "-" (R : AlertCountType) return AlertCountType ;
  impure function SumAlertCount(AlertCount: AlertCountType) return integer ;
  impure function GetAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return AlertCountType ;
  impure function GetAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return integer ;
  impure function GetEnabledAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return AlertCountType ;
  impure function GetEnabledAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return integer ;
  impure function GetDisabledAlertCount return AlertCountType ;
  impure function GetDisabledAlertCount return integer ;
  impure function GetDisabledAlertCount(AlertLogID: AlertLogIDType) return AlertCountType ;
  impure function GetDisabledAlertCount(AlertLogID: AlertLogIDType) return integer ;

  ------------------------------------------------------------
  --  log filtering for verbosity control, optionally has a separate file parameter
  procedure Log( 
    AlertLogID   : AlertLogIDType ; 
    Message      : string ;
    Level        : LogType := ALWAYS ;
    Enable       : boolean := FALSE    -- override internal enable
  ) ; 
  procedure Log( Message : string ; Level : LogType := ALWAYS ; Enable : boolean := FALSE) ;

  
  ------------------------------------------------------------
  -- Accessor Methods
  procedure SetAlertLogName(Name : string ) ;
  impure function GetAlertLogName(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return string ;
  procedure DeallocateAlertLogStruct ;
  procedure InitializeAlertLogStruct ;
  impure function FindAlertLogID(Name : string ) return AlertLogIDType ;
  impure function FindAlertLogID(Name : string ; ParentID : AlertLogIDType) return AlertLogIDType ;
  impure function GetAlertLogID(Name : string ; ParentID : AlertLogIDType := ALERTLOG_BASE_ID ; CreateHierarchy : Boolean := TRUE) return AlertLogIDType ;
  impure function GetAlertLogParentID(AlertLogID : AlertLogIDType) return AlertLogIDType ;

  ------------------------------------------------------------
  -- Accessor Methods
  procedure SetGlobalAlertEnable (A : boolean := TRUE) ;
  impure function SetGlobalAlertEnable (A : boolean := TRUE) return boolean ;
  impure function GetGlobalAlertEnable return boolean ;
  procedure IncAffirmCheckCount ;
  impure function GetAffirmCheckCount return natural ;
--??  procedure IncAffirmPassCount ;
--??  impure function GetAffirmPassCount return natural ;
  
  procedure SetAlertStopCount(AlertLogID : AlertLogIDType ;  Level : AlertType ;  Count : integer) ;
  procedure SetAlertStopCount(Level : AlertType ;  Count : integer) ;
  impure function GetAlertStopCount(AlertLogID : AlertLogIDType ;  Level : AlertType) return integer ;
  impure function GetAlertStopCount(Level : AlertType) return integer ;

  procedure SetAlertEnable(Level : AlertType ;  Enable : boolean) ; 
  procedure SetAlertEnable(AlertLogID : AlertLogIDType ;  Level : AlertType ;  Enable : boolean ; DescendHierarchy : boolean := TRUE) ;
  impure function GetAlertEnable(AlertLogID : AlertLogIDType ;  Level : AlertType) return boolean ;
  impure function GetAlertEnable(Level : AlertType) return boolean ;

  procedure SetLogEnable(Level : LogType ;  Enable : boolean) ; 
  procedure SetLogEnable(AlertLogID : AlertLogIDType ;  Level : LogType ;  Enable : boolean ; DescendHierarchy : boolean := TRUE) ;
  impure function GetLogEnable(AlertLogID : AlertLogIDType ;  Level : LogType) return boolean ;
  impure function GetLogEnable(Level : LogType) return boolean ;
  impure function IsLoggingEnabled(AlertLogID : AlertLogIDType ; Level : LogType) return boolean ;  -- same as GetLogEnable
  impure function IsLoggingEnabled(Level : LogType) return boolean ; 
  
  procedure ReportLogEnables ;
  
  ------------------------------------------------------------
  procedure SetAlertLogOptions (
    FailOnWarning         : AlertLogOptionsType := OPT_INIT_PARM_DETECT ; 
    FailOnDisabledErrors  : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    ReportHierarchy       : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteAlertLevel       : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteAlertName        : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteAlertTime        : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteLogLevel         : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteLogName          : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteLogTime          : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    AlertPrefix           : string := OSVVM_STRING_INIT_PARM_DETECT ;
    LogPrefix             : string := OSVVM_STRING_INIT_PARM_DETECT ;
    ReportPrefix          : string := OSVVM_STRING_INIT_PARM_DETECT ;
    DoneName              : string := OSVVM_STRING_INIT_PARM_DETECT ;
    PassName              : string := OSVVM_STRING_INIT_PARM_DETECT ;
    FailName              : string := OSVVM_STRING_INIT_PARM_DETECT 
  ) ;
  
  procedure ReportAlertLogOptions ;
  
  impure function GetAlertLogFailOnWarning        return AlertLogOptionsType ;
  impure function GetAlertLogFailOnDisabledErrors return AlertLogOptionsType ;
  impure function GetAlertLogReportHierarchy      return AlertLogOptionsType ;
  impure function GetAlertLogFoundReportHier      return boolean ;
  impure function GetAlertLogFoundAlertHier       return boolean ;
  impure function GetAlertLogWriteAlertLevel      return AlertLogOptionsType ;
  impure function GetAlertLogWriteAlertName       return AlertLogOptionsType ;
  impure function GetAlertLogWriteAlertTime       return AlertLogOptionsType ;
  impure function GetAlertLogWriteLogLevel        return AlertLogOptionsType ;
  impure function GetAlertLogWriteLogName         return AlertLogOptionsType ;
  impure function GetAlertLogWriteLogTime         return AlertLogOptionsType ;

  impure function GetAlertLogAlertPrefix          return string ;
  impure function GetAlertLogLogPrefix            return string ;
  
  impure function GetAlertLogReportPrefix         return string ;
  impure function GetAlertLogDoneName             return string ;
  impure function GetAlertLogPassName             return string ;
  impure function GetAlertLogFailName             return string ;

  
  -- File Reading Utilities
  function IsLogEnableType (Name : String) return boolean ;
  procedure ReadLogEnables (file AlertLogInitFile : text) ;
  procedure ReadLogEnables (FileName : string) ;

  -- String Helper Functions -- This should be in a more general string package
  function PathTail (A : string) return string ; 

end AlertLogPkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

use work.NamePkg.all ;

package body AlertLogPkg is

  -- instead of justify(to_upper(to_string())), just look up the upper case, left justified values
  type     AlertNameType is array(AlertType) of string(1 to 7) ; 
  constant ALERT_NAME : AlertNameType := (WARNING => "WARNING", ERROR => "ERROR  ", FAILURE => "FAILURE") ;  -- , NEVER => "NEVER  "
  type     LogNameType is array(LogType) of string(1 to 7) ; 
  constant LOG_NAME : LogNameType := (DEBUG => "DEBUG  ", FINAL => "FINAL  ", INFO => "INFO   ", ALWAYS => "ALWAYS ", PASSED => "PASSED ") ; -- , NEVER => "NEVER  "


  type AlertLogStructPType is protected
  
    ------------------------------------------------------------
    procedure alert ( 
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ; 
      message      : string ;
      level        : AlertType := ERROR
    ) ;
    
    ------------------------------------------------------------
    procedure IncAlertCount ( AlertLogID : AlertLogIDType ; level : AlertType := ERROR ) ;
    procedure SetJustify ;
    procedure ReportAlerts ( Name : string ; AlertCount : AlertCountType ) ;
    procedure ReportAlerts ( Name : string := OSVVM_STRING_INIT_PARM_DETECT ; AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID ; ExternalErrors : AlertCountType := (0,0,0) ; ReportAll : boolean := TRUE ) ;
    procedure ClearAlerts ; 
    impure function GetAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return AlertCountType ;
    impure function GetEnabledAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return AlertCountType ;
    impure function GetDisabledAlertCount return AlertCountType ;
    impure function GetDisabledAlertCount(AlertLogID: AlertLogIDType) return AlertCountType ;
    
    ------------------------------------------------------------
    procedure log ( 
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ; 
      Message      : string ;
      Level        : LogType := ALWAYS ;
      Enable       : boolean := FALSE    -- override internal enable
    ) ;
    
    ------------------------------------------------------------
    -- FILE IO Controls
--    procedure SetTranscriptEnable (A : boolean := TRUE) ;
--    impure function IsTranscriptEnabled return boolean ;
--    procedure MirrorTranscript (A : boolean := TRUE) ;
--    impure function IsTranscriptMirrored return boolean ;

    ------------------------------------------------------------
    ------------------------------------------------------------
    -- AlertLog Structure Creation and Interaction Methods

    ------------------------------------------------------------
    procedure SetAlertLogName(Name : string ) ;
    procedure SetNumAlertLogIDs (NewNumAlertLogIDs : integer) ;
    impure function FindAlertLogID(Name : string ) return AlertLogIDType ;
    impure function FindAlertLogID(Name : string ; ParentID : AlertLogIDType) return AlertLogIDType ;
    impure function GetAlertLogID(Name : string ; ParentID : AlertLogIDType ; CreateHierarchy : Boolean) return AlertLogIDType ;
    impure function GetAlertLogParentID(AlertLogID : AlertLogIDType) return AlertLogIDType ;
    procedure Initialize(NewNumAlertLogIDs : integer := MIN_NUM_AL_IDS) ;
    procedure Deallocate ;

    ------------------------------------------------------------
    ------------------------------------------------------------
    -- Accessor Methods
    ------------------------------------------------------------
    procedure SetGlobalAlertEnable (A : boolean := TRUE) ;
    impure function GetAlertLogName(AlertLogID : AlertLogIDType) return string ;
    impure function GetGlobalAlertEnable return boolean ;
    procedure IncAffirmCheckCount ;
    impure function GetAffirmCheckCount return natural ;
--??    procedure IncAffirmPassCount ;
--??    impure function GetAffirmPassCount return natural ;

    procedure SetAlertStopCount(AlertLogID : AlertLogIDType ;  Level : AlertType ;  Count : integer) ;
    impure function GetAlertStopCount(AlertLogID : AlertLogIDType ;  Level : AlertType) return integer ;

    procedure SetAlertEnable(Level : AlertType ;  Enable : boolean) ; 
    procedure SetAlertEnable(AlertLogID : AlertLogIDType ;  Level : AlertType ;  Enable : boolean ; DescendHierarchy : boolean := TRUE) ;
    impure function GetAlertEnable(AlertLogID : AlertLogIDType ;  Level : AlertType) return boolean ;

    procedure SetLogEnable(Level : LogType ;  Enable : boolean) ; 
    procedure SetLogEnable(AlertLogID : AlertLogIDType ;  Level : LogType ;  Enable : boolean ; DescendHierarchy : boolean := TRUE) ;
    impure function GetLogEnable(AlertLogID : AlertLogIDType ;  Level : LogType) return boolean ;
    
    procedure ReportLogEnables ;

    ------------------------------------------------------------
    -- Reporting Accessor
    procedure SetAlertLogOptions (
      FailOnWarning         : AlertLogOptionsType := OPT_INIT_PARM_DETECT ; 
      FailOnDisabledErrors  : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      ReportHierarchy       : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteAlertLevel       : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteAlertName        : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteAlertTime        : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteLogLevel         : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteLogName          : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteLogTime          : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      AlertPrefix           : string := OSVVM_STRING_INIT_PARM_DETECT ;
      LogPrefix             : string := OSVVM_STRING_INIT_PARM_DETECT ;
      ReportPrefix          : string := OSVVM_STRING_INIT_PARM_DETECT ;
      DoneName              : string := OSVVM_STRING_INIT_PARM_DETECT ;
      PassName              : string := OSVVM_STRING_INIT_PARM_DETECT ;
      FailName              : string := OSVVM_STRING_INIT_PARM_DETECT 
    ) ;
    procedure ReportAlertLogOptions ;
    
    impure function GetAlertLogFailOnWarning        return AlertLogOptionsType ;
    impure function GetAlertLogFailOnDisabledErrors return AlertLogOptionsType ;
    impure function GetAlertLogReportHierarchy      return AlertLogOptionsType ;
    impure function GetAlertLogFoundReportHier      return boolean ;
    impure function GetAlertLogFoundAlertHier       return boolean ;
    impure function GetAlertLogWriteAlertLevel      return AlertLogOptionsType ;
    impure function GetAlertLogWriteAlertName       return AlertLogOptionsType ;
    impure function GetAlertLogWriteAlertTime       return AlertLogOptionsType ;
    impure function GetAlertLogWriteLogLevel        return AlertLogOptionsType ;
    impure function GetAlertLogWriteLogName         return AlertLogOptionsType ;
    impure function GetAlertLogWriteLogTime         return AlertLogOptionsType ;

    impure function GetAlertLogAlertPrefix          return string ;
    impure function GetAlertLogLogPrefix            return string ;

    impure function GetAlertLogReportPrefix return string ;
    impure function GetAlertLogDoneName return string ;
    impure function GetAlertLogPassName return string ;
    impure function GetAlertLogFailName return string ;

  end  protected AlertLogStructPType ; 

  --- ///////////////////////////////////////////////////////////////////////////
  
  type AlertLogStructPType is protected body
  
    variable GlobalAlertEnabledVar     : boolean := TRUE ; -- Allows turn off and on
    
    variable AffirmCheckCountVar       : natural := 0 ; 
--??    variable AffirmPassedCountVar      : natural := 0 ; 

    ------------------------------------------------------------
    type AlertLogRecType is record
    ------------------------------------------------------------
      Name              : Line ; 
      ParentID          : AlertLogIDType ; 
      AlertCount        : AlertCountType ; 
      AlertStopCount    : AlertCountType ;
      AlertEnabled      : AlertEnableType ;
      LogEnabled        : LogEnableType ; 
    end record AlertLogRecType ; 
    
    ------------------------------------------------------------
    -- Basis for AlertLog Data Structure 
    variable NumAlertLogIDsVar          : AlertLogIDType := 0 ; -- defined by initialize 
    variable NumAllocatedAlertLogIDsVar : AlertLogIDType := 0 ; 
--xx    variable NumPredefinedAlIDsVar      : AlertLogIDType := 0 ; -- defined by initialize 
    
    type AlertLogRecPtrType   is access AlertLogRecType ; 
    type AlertLogArrayType    is array (AlertLogIDType range <>) of AlertLogRecPtrType ; 
    type AlertLogArrayPtrType is access AlertLogArrayType ;
    variable AlertLogPtr  : AlertLogArrayPtrType ;
    
    ------------------------------------------------------------
    -- Report formatting settings, with defaults
    variable FailOnWarningVar          : boolean := TRUE ;
    variable FailOnDisabledErrorsVar   : boolean := TRUE ;
    variable ReportHierarchyVar        : boolean := TRUE ;
    variable FoundReportHierVar        : boolean := FALSE ;
    variable FoundAlertHierVar         : boolean := FALSE ;

    variable WriteAlertLevelVar        : boolean := TRUE ;
    variable WriteAlertNameVar         : boolean := TRUE ;
    variable WriteAlertTimeVar         : boolean := TRUE ;
    variable WriteLogLevelVar          : boolean := TRUE ;
    variable WriteLogNameVar           : boolean := TRUE ;
    variable WriteLogTimeVar           : boolean := TRUE ;

    variable AlertPrefixVar            : NamePType ;
    variable LogPrefixVar              : NamePType ;
    variable ReportPrefixVar           : NamePType ;
    variable DoneNameVar               : NamePType ;
    variable PassNameVar               : NamePType ;
    variable FailNameVar               : NamePType ;

    variable AlertLogJustifyAmountVar  : integer := 0 ; 
    variable ReportJustifyAmountVar    : integer := 0 ; 
                  
    ------------------------------------------------------------
    -- PT Local
    impure function LeftJustify(A : String;  Amount : integer) return string is
    ------------------------------------------------------------
      constant Spaces : string(1 to  maximum(1, Amount)) := (others => ' ') ; 
    begin
      if A'length >= Amount then
        return A ; 
      else 
        return A & Spaces(1 to Amount - A'length) ;
      end if ; 
    end function LeftJustify ; 


    ------------------------------------------------------------
    -- PT Local
    procedure IncrementAlertCount(
    ------------------------------------------------------------
      constant AlertLogID     : in    AlertLogIDType ;
      constant Level          : in    AlertType ;
      variable StopDueToCount : inout boolean 
    ) is 
    begin
      -- Always Count at this level
      AlertLogPtr(AlertLogID).AlertCount(Level) := AlertLogPtr(AlertLogID).AlertCount(Level) + 1 ;
      -- Only do remaining actions if enabled
      if AlertLogPtr(AlertLogID).AlertEnabled(Level) then 
        -- Exceeded Stop Count at this level?
        if AlertLogPtr(AlertLogID).AlertCount(Level) >= AlertLogPtr(AlertLogID).AlertStopCount(Level) then 
          StopDueToCount := TRUE ; 
        end if ; 
        -- Propagate counts to parent(s)  -- Ascend Hierarchy
        if AlertLogID /= ALERTLOG_BASE_ID then
          IncrementAlertCount(AlertLogPtr(AlertLogID).ParentID, Level, StopDueToCount) ; 
        end if ; 
      end if ; 
    end procedure IncrementAlertCount ; 

    ------------------------------------------------------------
    procedure alert ( 
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ; 
      message      : string ;
      level        : AlertType := ERROR
    ) is
      variable buf : Line ; 
      constant AlertPrefix : string := AlertPrefixVar.Get(OSVVM_DEFAULT_ALERT_PREFIX) ;
      variable StopDueToCount : boolean := FALSE ; 
    begin
      if GlobalAlertEnabledVar then
        -- Do not write or count when GlobalAlertEnabledVar is disabled
        if AlertLogPtr(AlertLogID).AlertEnabled(Level) then
          -- do not write when disabled
          write(buf, AlertPrefix) ; 
          if WriteAlertLevelVar then 
            -- write(buf, " " & to_string(Level) ) ; 
            write(buf, " " & ALERT_NAME(Level)) ; -- uses constant lookup
          end if ; 
--xx          if  (NumAlertLogIDsVar > NumPredefinedAlIDsVar) and WriteAlertNameVar then -- print hierarchy names even when silent
          if FoundAlertHierVar and WriteAlertNameVar then 
--            write(buf, " in " & justify(AlertLogPtr(AlertLogID).Name.all & ",", LEFT, AlertLogJustifyAmountVar) ) ; 
            write(buf, " in " & LeftJustify(AlertLogPtr(AlertLogID).Name.all & ",", AlertLogJustifyAmountVar) ) ; 
          end if ; 
          write(buf, " " & Message) ; 
          if WriteAlertTimeVar then 
             write(buf, " at " & to_string(NOW, 1 ns)) ; 
          end if ; 
          writeline(buf) ; 
        end if ; 
        -- Always Count
        IncrementAlertCount(AlertLogID, Level, StopDueToCount) ;
        if StopDueToCount then 
          write(buf, LF & AlertPrefix & " Stop Count on " & ALERT_NAME(Level) & " reached") ; 
--xx          if NumAlertLogIDsVar > NumPredefinedAlIDsVar then -- print hierarchy names even when silent
          if FoundAlertHierVar then 
            write(buf, " in " & AlertLogPtr(AlertLogID).Name.all) ; 
          end if ; 
          write(buf, " at " & to_string(NOW, 1 ns) & " ") ; 
          writeline(buf) ; 
          ReportAlerts(ReportAll => TRUE) ;
          std.env.stop(1) ; 
        end if ; 
      end if ; 
    end procedure alert ; 
    
    ------------------------------------------------------------
    procedure IncAlertCount ( 
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ; 
      level        : AlertType := ERROR
    ) is
      variable buf : Line ; 
      constant AlertPrefix : string := AlertPrefixVar.Get(OSVVM_DEFAULT_ALERT_PREFIX) ;
      variable StopDueToCount : boolean := FALSE ; 
    begin
      if GlobalAlertEnabledVar then
        IncrementAlertCount(AlertLogID, Level, StopDueToCount) ;
        if StopDueToCount then 
          write(buf, LF & AlertPrefix & " Stop Count on " & ALERT_NAME(Level) & " reached") ; 
--xx          if NumAlertLogIDsVar > NumPredefinedAlIDsVar then -- print hierarchy names even when silent
           if FoundAlertHierVar then 
            write(buf, " in " & AlertLogPtr(AlertLogID).Name.all) ; 
          end if ; 
          write(buf, " at " & to_string(NOW, 1 ns) & " ") ; 
          writeline(buf) ; 
          ReportAlerts(ReportAll => TRUE) ;
          std.env.stop ; 
        end if ; 
      end if ; 
    end procedure IncAlertCount ; 
    
    ------------------------------------------------------------
    -- PT Local
    impure function CalcJustify (AlertLogID : AlertLogIDType ; CurrentLength : integer ; IndentAmount : integer) return integer_vector is
    ------------------------------------------------------------
      variable ResultValues, LowerLevelValues : integer_vector(1 to 2) ;  -- 1 = Max, 2 = Indented 
    begin
      ResultValues(1) := CurrentLength + 1 ;            -- AlertLogJustifyAmountVar
      ResultValues(2) := CurrentLength + IndentAmount ; -- ReportJustifyAmountVar
      for i in AlertLogID+1 to NumAlertLogIDsVar loop 
        if AlertLogID = AlertLogPtr(i).ParentID then 
          LowerLevelValues := CalcJustify(i, AlertLogPtr(i).Name'length, IndentAmount + 2) ;
          ResultValues(1)  := maximum(ResultValues(1), LowerLevelValues(1)) ; 
          ResultValues(2)  := maximum(ResultValues(2), LowerLevelValues(2)) ; 
        end if ; 
      end loop ; 
      return ResultValues ; 
    end function CalcJustify ;

    ------------------------------------------------------------
    procedure SetJustify is
    ------------------------------------------------------------
      variable ResultValues : integer_vector(1 to 2) ;  -- 1 = Max, 2 = Indented 
    begin
      ResultValues := CalcJustify(ALERTLOG_BASE_ID, 0, 0) ; 
      AlertLogJustifyAmountVar := ResultValues(1) ; 
      ReportJustifyAmountVar   := ResultValues(2) ; 
    end procedure SetJustify ;

    ------------------------------------------------------------
    -- PT Local
    impure function GetEnabledAlertCount(AlertCount: AlertCountType;  AlertEnabled : AlertEnableType) return AlertCountType is
    ------------------------------------------------------------
      variable Count : AlertCountType := (others => 0) ; 
    begin
      if AlertEnabled(FAILURE) then 
        Count(FAILURE) := AlertCount(FAILURE) ; 
      end if ; 
      if AlertEnabled(ERROR) then 
        Count(ERROR) := AlertCount(ERROR) ; 
      end if ; 
      if FailOnWarningVar and AlertEnabled(WARNING) then 
        Count(WARNING) := AlertCount(WARNING) ; 
      end if ;
      return Count ; 
    end function GetEnabledAlertCount ; 

    ------------------------------------------------------------
    impure function GetAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return AlertCountType is
    ------------------------------------------------------------
      variable AlertCount : AlertCountType ; 
    begin
      return AlertLogPtr(AlertLogID).AlertCount ; 
    end function GetAlertCount ; 

    ------------------------------------------------------------
    impure function GetEnabledAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return AlertCountType is
    ------------------------------------------------------------
      variable AlertCount : AlertCountType ; 
    begin
      return GetEnabledAlertCount(AlertLogPtr(AlertLogID).AlertCount, AlertLogPtr(AlertLogID).AlertEnabled) ;
    end function GetEnabledAlertCount ; 
    
    ------------------------------------------------------------
    -- PT Local
    impure function GetDisabledAlertCount(AlertCount: AlertCountType;  AlertEnabled : AlertEnableType) return AlertCountType is
    ------------------------------------------------------------
      variable Count : AlertCountType := (others => 0) ; 
    begin
      if not AlertEnabled(FAILURE) then 
        Count(FAILURE) := AlertCount(FAILURE) ; 
      end if ; 
      if not AlertEnabled(ERROR) then 
        Count(ERROR) := AlertCount(ERROR) ; 
      end if ; 
      if FailOnWarningVar and not AlertEnabled(WARNING) then 
        Count(WARNING) := AlertCount(WARNING) ; 
      end if ;
      return Count ; 
    end function GetDisabledAlertCount ; 

    ------------------------------------------------------------
    impure function GetDisabledAlertCount return AlertCountType is
    ------------------------------------------------------------
      variable Count : AlertCountType := (others => 0) ; 
    begin
      for i in ALERTLOG_BASE_ID to NumAlertLogIDsVar loop 
        Count := Count + GetDisabledAlertCount(AlertLogPtr(i).AlertCount, AlertLogPtr(i).AlertEnabled) ;  
      end loop ;
      return Count ; 
    end function GetDisabledAlertCount ; 

    ------------------------------------------------------------
    impure function GetDisabledAlertCount(AlertLogID: AlertLogIDType) return AlertCountType is
    ------------------------------------------------------------
      variable Count : AlertCountType := (others => 0) ; 
    begin
      Count := GetDisabledAlertCount(AlertLogPtr(AlertLogID).AlertCount, AlertLogPtr(AlertLogID).AlertEnabled) ; 
      for i in AlertLogID+1 to NumAlertLogIDsVar loop 
        if AlertLogID = AlertLogPtr(i).ParentID then 
          Count := Count + GetDisabledAlertCount(i) ;
        end if ; 
      end loop ; 
      return Count ; 
    end function GetDisabledAlertCount ;
    
    ------------------------------------------------------------
    -- PT Local
    procedure PrintTopAlerts ( 
    ------------------------------------------------------------
      NumErrors          : integer ;
      AlertCount         : AlertCountType ; 
      Name               : string ; 
      NumDisabledErrors  : integer  
    ) is
      constant ReportPrefix    : string := ResolveOsvvmWritePrefix(ReportPrefixVar.GetOpt ) ;
      constant DoneName        : string := ResolveOsvvmDoneName(DoneNameVar.GetOpt     ) ;
      constant PassName        : string := ResolveOsvvmPassName(PassNameVar.GetOpt     ) ;
      constant FailName        : string := ResolveOsvvmFailName(FailNameVar.GetOpt     ) ;
      variable buf : line ; 
    begin
      if NumErrors = 0 then 
        if NumDisabledErrors = 0 then 
          -- Passed
          write(buf, ReportPrefix & DoneName & "  " & PassName & "  " & Name) ; 
          if AffirmCheckCountVar > 0 then 
            write(buf, "  Affirmations Checked: " & to_string(AffirmCheckCountVar)) ; 
          end if ;
          write(buf, "  at "  & to_string(NOW, 1 ns)) ;
          WriteLine(buf) ; 
        else
          -- Failed Due to Disabled Errors
          write(buf, ReportPrefix & DoneName & "  " & FailName & "  " & Name) ; 
          write(buf, "   Failed Due to Disabled Error(s) = " & to_string(NumDisabledErrors)) ;
          if AffirmCheckCountVar > 0 then 
            write(buf, "  Affirmations Checked: " & to_string(AffirmCheckCountVar)) ; 
          end if ;
          write(buf, "  at "  & to_string(NOW, 1 ns)) ;
          WriteLine(buf) ; 
        end if ; 
      else 
        -- Failed 
        write(buf, ReportPrefix & DoneName & "  " & FailName & "  "& Name) ; 
        write(buf, "  Total Error(s) = "      & to_string(NumErrors) ) ;
        write(buf, "  Failures: "  & to_string(AlertCount(FAILURE)) ) ;
        write(buf, "  Errors: "    & to_string(AlertCount(ERROR) ) ) ;
        write(buf, "  Warnings: "  & to_string(AlertCount(WARNING) ) ) ;
        if AffirmCheckCountVar > 0 then 
--??         write(buf, "  Affirmations Passed: " & to_string(AffirmPassedCountVar)) ;
--??          write(buf, "  Checked: " & to_string(AffirmCheckCountVar)) ; 
          write(buf, "  Affirmations Checked: " & to_string(AffirmCheckCountVar)) ; 
        end if ; 
        Write(buf, "  at "  & to_string(NOW, 1 ns)) ;
        WriteLine(buf) ; 
      end if ; 
    end procedure PrintTopAlerts ; 

    ------------------------------------------------------------
    -- PT Local
    procedure PrintChild(
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ;  
      Prefix       : string ;
      IndentAmount : integer ;
      ReportAll    : boolean 
    ) is 
      variable buf : line ; 
    begin
      for i in AlertLogID+1 to NumAlertLogIDsVar loop 
        if AlertLogID = AlertLogPtr(i).ParentID then 
          if ReportAll or SumAlertCount(AlertLogPtr(i).AlertCount) > 0 then 
            Write(buf, Prefix &  " "   & LeftJustify(AlertLogPtr(i).Name.all, ReportJustifyAmountVar - IndentAmount)) ;        
            write(buf, "  Failures: "  & to_string(AlertLogPtr(i).AlertCount(FAILURE) ) ) ;
            write(buf, "  Errors: "    & to_string(AlertLogPtr(i).AlertCount(ERROR) ) ) ;
            write(buf, "  Warnings: "  & to_string(AlertLogPtr(i).AlertCount(WARNING) ) ) ;
            WriteLine(buf) ; 
          end if ; 
          PrintChild(
            AlertLogID    => i, 
            Prefix        => Prefix & "  ", 
            IndentAmount  => IndentAmount + 2,
            ReportAll     => ReportAll
          ) ; 
        end if ; 
      end loop ; 
    end procedure PrintChild ; 

    ------------------------------------------------------------
    procedure ReportAlerts ( Name : string := OSVVM_STRING_INIT_PARM_DETECT ; AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID ; ExternalErrors : AlertCountType := (0,0,0) ; ReportAll : boolean := TRUE) is
    ------------------------------------------------------------
      variable NumErrors : integer ; 
      variable NumDisabledErrors : integer ; 
      constant ReportPrefix : string := ResolveOsvvmWritePrefix(ReportPrefixVar.GetOpt) ;
    begin
      if ReportJustifyAmountVar <= 0 then
        SetJustify ; 
      end if ; 
      NumErrors := SumAlertCount(  ExternalErrors + GetEnabledAlertCount(AlertLogPtr(AlertLogID).AlertCount, AlertLogPtr(AlertLogID).AlertEnabled) ) ;
      if FailOnDisabledErrorsVar then
        NumDisabledErrors := SumAlertCount( GetDisabledAlertCount(AlertLogID) ) ; 
      else
        NumDisabledErrors := 0 ;
      end if ;     
      if IsOsvvmStringSet(Name) then 
        PrintTopAlerts ( 
          NumErrors          => NumErrors, 
          AlertCount         => AlertLogPtr(AlertLogID).AlertCount + ExternalErrors,
          Name               => Name,
          NumDisabledErrors  => NumDisabledErrors
        ) ;
      else
        PrintTopAlerts ( 
          NumErrors          => NumErrors, 
          AlertCount         => AlertLogPtr(AlertLogID).AlertCount + ExternalErrors,
          Name               => AlertLogPtr(AlertLogID).Name.all,
          NumDisabledErrors  => NumDisabledErrors
        ) ;
      end if ; 
      --Print Hierarchy when enabled and error or disabled error
      if (FoundReportHierVar and ReportHierarchyVar) and (NumErrors /= 0 or NumDisabledErrors /=0) then
        PrintChild(
          AlertLogID    => AlertLogID, 
          Prefix        => ReportPrefix & "  ", 
          IndentAmount  => 2,
          ReportAll     => ReportAll
        ) ; 
      end if ; 
    end procedure ReportAlerts ; 
    
    ------------------------------------------------------------
    procedure ReportAlerts ( Name : string ; AlertCount : AlertCountType ) is
    ------------------------------------------------------------
    begin
       PrintTopAlerts ( 
        NumErrors          => SumAlertCount(AlertCount), 
        AlertCount         => AlertCount,
        Name               => Name,
        NumDisabledErrors  => 0
      ) ;
    end procedure ReportAlerts ; 
  
    ------------------------------------------------------------
    procedure ClearAlerts is
    ------------------------------------------------------------
    begin
      AffirmCheckCountVar  := 0 ; 
--??      AffirmPassedCountVar := 0 ; 

      AlertLogPtr(ALERTLOG_BASE_ID).AlertCount := (0, 0, 0) ;
      AlertLogPtr(ALERTLOG_BASE_ID).AlertStopCount := (FAILURE => 0, ERROR => integer'right, WARNING => integer'right) ; 

      for i in ALERTLOG_BASE_ID + 1 to NumAlertLogIDsVar loop 
        AlertLogPtr(i).AlertCount := (0, 0, 0) ; 
        AlertLogPtr(i).AlertStopCount := (FAILURE => integer'right, ERROR => integer'right, WARNING => integer'right) ; 
      end loop ; 
    end procedure ClearAlerts ; 

    ------------------------------------------------------------
    -- PT Local
    procedure LocalLog ( 
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ; 
      Message      : string ;
      Level        : LogType  
    ) is
      variable buf : line ; 
      constant LogPrefix : string := LogPrefixVar.Get(OSVVM_DEFAULT_LOG_PREFIX) ;
    begin
      write(buf, LogPrefix) ; 
      if WriteLogLevelVar then 
        write(buf, " " & LOG_NAME(Level) ) ; 
      end if ; 
--xx      if (NumAlertLogIDsVar > NumPredefinedAlIDsVar) and WriteLogNameVar then -- print hierarchy names even when silent
      if FoundAlertHierVar and WriteLogNameVar then 
--        write(buf, " in " & justify(AlertLogPtr(AlertLogID).Name.all & ",", LEFT, AlertLogJustifyAmountVar) ) ; 
        write(buf, " in " & LeftJustify(AlertLogPtr(AlertLogID).Name.all & ",", AlertLogJustifyAmountVar) ) ; 
      end if ; 
      write(buf, " " & Message) ; 
      if WriteLogTimeVar then 
         write(buf, " at " & to_string(NOW, 1 ns)) ; 
      end if ; 
      writeline(buf) ; 
    end procedure LocalLog ;     

    ------------------------------------------------------------
    procedure log ( 
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ; 
      Message      : string ;
      Level        : LogType := ALWAYS ;
      Enable       : boolean := FALSE    -- override internal enable
    ) is
    begin
      if Level = ALWAYS or Enable then 
        LocalLog(AlertLogID, Message, Level) ; 
      elsif AlertLogPtr(AlertLogID).LogEnabled(Level) then
        LocalLog(AlertLogID, Message, Level) ; 
      end if ; 
    end procedure log ;     
       
    ------------------------------------------------------------
    ------------------------------------------------------------
    -- AlertLog Structure Creation and Interaction Methods
        
    ------------------------------------------------------------
    procedure SetAlertLogName(Name : string ) is
    ------------------------------------------------------------
    begin
      Deallocate(AlertLogPtr(ALERTLOG_BASE_ID).Name) ; 
      AlertLogPtr(ALERTLOG_BASE_ID).Name := new string'(Name) ; 
    end procedure SetAlertLogName ; 

    ------------------------------------------------------------
    impure function GetAlertLogName(AlertLogID : AlertLogIDType) return string is
    ------------------------------------------------------------
    begin
      return AlertLogPtr(AlertLogID).Name.all ;
    end function GetAlertLogName ; 
    
    ------------------------------------------------------------
    -- PT Local
    procedure NewAlertLogRec(AlertLogID : AlertLogIDType ; Name : string ; ParentID : AlertLogIDType) is
    ------------------------------------------------------------
      variable AlertEnabled   : AlertEnableType ; 
      variable AlertStopCount : AlertCountType ; 
      variable LogEnabled     : LogEnableType ; 
    begin
      if AlertLogID = ALERTLOG_BASE_ID then 
        AlertEnabled := (TRUE, TRUE, TRUE) ; 
        LogEnabled   := (others => FALSE) ;
        AlertStopCount := (FAILURE => 0, ERROR => integer'right, WARNING => integer'right) ; 
      else
        if ParentID < ALERTLOG_BASE_ID then
          AlertEnabled := AlertLogPtr(ALERTLOG_BASE_ID).AlertEnabled ; 
          LogEnabled   := AlertLogPtr(ALERTLOG_BASE_ID).LogEnabled ;
        else
          AlertEnabled := AlertLogPtr(ParentID).AlertEnabled ; 
          LogEnabled   := AlertLogPtr(ParentID).LogEnabled ;
        end if ; 
        AlertStopCount := (FAILURE => integer'right, ERROR => integer'right, WARNING => integer'right) ; 
      end if ; 
      AlertLogPtr(AlertLogID) := new AlertLogRecType ; 
      AlertLogPtr(AlertLogID).Name              := new string'(NAME) ;
      AlertLogPtr(AlertLogID).ParentID          := ParentID ;  
      AlertLogPtr(AlertLogID).AlertCount        := (0, 0, 0) ;
      AlertLogPtr(AlertLogID).AlertEnabled      := AlertEnabled ; 
      AlertLogPtr(AlertLogID).AlertStopCount    := AlertStopCount ;
      AlertLogPtr(AlertLogID).LogEnabled        := LogEnabled ;
--      AlertLogPtr(AlertLogID) := new AlertLogRecType'(
--          Name              => new string'(NAME),
--          ParentID          => ParentID,  
--          AlertCount        => (0, 0, 0),
--          AlertEnabled      => AlertEnabled, 
--          AlertStopCount    => AlertStopCount,
--          LogEnabled        => LogEnabled
--        ) ; 
    end procedure NewAlertLogRec ;
    
    ------------------------------------------------------------
    -- PT Local  
    -- Construct initial data structure
    procedure LocalInitialize(NewNumAlertLogIDs : integer := MIN_NUM_AL_IDS) is
    ------------------------------------------------------------
    begin
      if NumAllocatedAlertLogIDsVar /= 0 then
        Alert(ALERT_DEFAULT_ID, "AlertLogPkg: Initialize, data structure already initialized", FAILURE) ; 
        return ; 
      end if ; 
      -- Initialize Pointer
      AlertLogPtr := new AlertLogArrayType(ALERTLOG_BASE_ID to ALERTLOG_BASE_ID + NewNumAlertLogIDs) ;
      NumAllocatedAlertLogIDsVar := NewNumAlertLogIDs ;
      -- Create BASE AlertLogID (if it differs from DEFAULT
      if ALERTLOG_BASE_ID /= ALERT_DEFAULT_ID then
        NewAlertLogRec(ALERTLOG_BASE_ID, "AlertLogTop", ALERTLOG_BASE_ID) ;
      end if ; 
      -- Create DEFAULT AlertLogID
      NewAlertLogRec(ALERT_DEFAULT_ID, "Default", ALERTLOG_BASE_ID) ;
      NumAlertLogIDsVar := ALERT_DEFAULT_ID ; 
      -- Create OSVVM AlertLogID (if it differs from DEFAULT
      if OSVVM_ALERTLOG_ID /= ALERT_DEFAULT_ID then
        NewAlertLogRec(OSVVM_ALERTLOG_ID, "OSVVM", ALERTLOG_BASE_ID) ;
        NumAlertLogIDsVar := NumAlertLogIDsVar + 1 ; 
      end if ; 
      if OSVVM_SCOREBOARD_ALERTLOG_ID /= OSVVM_ALERTLOG_ID then
        NewAlertLogRec(OSVVM_SCOREBOARD_ALERTLOG_ID, "OSVVM Scoreboard", ALERTLOG_BASE_ID) ;
        NumAlertLogIDsVar := NumAlertLogIDsVar + 1 ; 
      end if ; 
    end procedure LocalInitialize ;
    
    ------------------------------------------------------------
    -- Construct initial data structure
    procedure Initialize(NewNumAlertLogIDs : integer := MIN_NUM_AL_IDS) is
    ------------------------------------------------------------
    begin
      LocalInitialize(NewNumAlertLogIDs) ;
    end procedure Initialize ; 
    
    ------------------------------------------------------------
    -- PT Local
    -- Constructs initial data structure using constant below 
    impure function LocalInitialize return boolean is
    ------------------------------------------------------------
    begin
      LocalInitialize(MIN_NUM_AL_IDS) ; 
      return TRUE ; 
    end function LocalInitialize ; 
    
    constant CONSTRUCT_ALERT_DATA_STRUCTURE : boolean := LocalInitialize ; 
    
    ------------------------------------------------------------
    procedure Deallocate is
    ------------------------------------------------------------
    begin
      for i in ALERTLOG_BASE_ID to NumAlertLogIDsVar loop 
        Deallocate(AlertLogPtr(i).Name) ; 
        Deallocate(AlertLogPtr(i)) ; 
      end loop ; 
      deallocate(AlertLogPtr) ;
      -- Free up space used by protected types within AlertLogPkg
      AlertPrefixVar.Deallocate ;
      LogPrefixVar.Deallocate ;
      ReportPrefixVar.Deallocate ;
      DoneNameVar.Deallocate ;
      PassNameVar.Deallocate ;
      FailNameVar.Deallocate ;
      -- Restore variables to their initial state
      NumAlertLogIDsVar           := 0 ; 
      NumAllocatedAlertLogIDsVar  := 0 ; 
      GlobalAlertEnabledVar    := TRUE ; -- Allows turn off and on
      AffirmCheckCountVar      := 0 ; 
--??      AffirmPassedCountVar     := 0 ; 
      FailOnWarningVar         := TRUE ;
      FailOnDisabledErrorsVar  := TRUE ;
      ReportHierarchyVar       := TRUE ;
      FoundReportHierVar       := FALSE ;
      FoundAlertHierVar        := FALSE ;
      WriteAlertLevelVar       := TRUE ;
      WriteAlertNameVar        := TRUE ;
      WriteAlertTimeVar        := TRUE ;
      WriteLogLevelVar         := TRUE ;
      WriteLogNameVar          := TRUE ;
      WriteLogTimeVar          := TRUE ;
    end procedure Deallocate ; 

    ------------------------------------------------------------
    -- PT Local.
    procedure GrowAlertStructure (NewNumAlertLogIDs : integer) is
    ------------------------------------------------------------
      variable oldAlertLogPtr : AlertLogArrayPtrType ;
    begin
      if NumAllocatedAlertLogIDsVar = 0 then
        Initialize (NewNumAlertLogIDs) ;   -- Construct initial structure
      else
        oldAlertLogPtr := AlertLogPtr ;
        AlertLogPtr := new AlertLogArrayType(ALERTLOG_BASE_ID to NewNumAlertLogIDs) ;
        AlertLogPtr(ALERTLOG_BASE_ID to NumAlertLogIDsVar) := oldAlertLogPtr(ALERTLOG_BASE_ID to NumAlertLogIDsVar) ;
        deallocate(oldAlertLogPtr) ;
      end if ;
      NumAllocatedAlertLogIDsVar := NewNumAlertLogIDs ; 
    end procedure GrowAlertStructure ;

    ------------------------------------------------------------
    -- Sets a AlertLogPtr to a particular size
    -- Use for small bins to save space or large bins to
    -- suppress the resize and copy as a CovBin autosizes.
    procedure SetNumAlertLogIDs (NewNumAlertLogIDs : integer) is
    ------------------------------------------------------------
      variable oldAlertLogPtr : AlertLogArrayPtrType ;
    begin
      if NewNumAlertLogIDs > NumAllocatedAlertLogIDsVar then 
        GrowAlertStructure(NewNumAlertLogIDs) ; 
      end if; 
    end procedure SetNumAlertLogIDs ;
    
    ------------------------------------------------------------
    -- PT Local
    impure function GetNextAlertLogID return AlertLogIDType is
    ------------------------------------------------------------
      variable NewNumAlertLogIDs : AlertLogIDType ;      
    begin
      NewNumAlertLogIDs := NumAlertLogIDsVar + 1 ; 
      if NewNumAlertLogIDs > NumAllocatedAlertLogIDsVar then
        GrowAlertStructure(NumAllocatedAlertLogIDsVar + MIN_NUM_AL_IDS) ; 
      end if ; 
      NumAlertLogIDsVar := NewNumAlertLogIDs ; 
      return NumAlertLogIDsVar ; 
    end function GetNextAlertLogID ;

    ------------------------------------------------------------
    impure function FindAlertLogID(Name : string ) return AlertLogIDType is
    ------------------------------------------------------------
    begin
      for i in ALERTLOG_BASE_ID to NumAlertLogIDsVar loop 
        if Name = AlertLogPtr(i).Name.all then
          return i ;
        end if ; 
      end loop ;
      return ALERTLOG_ID_NOT_FOUND ; -- not found
    end function FindAlertLogID ;
    
    ------------------------------------------------------------
    impure function FindAlertLogID(Name : string ; ParentID : AlertLogIDType) return AlertLogIDType is
    ------------------------------------------------------------
      variable CurParentID : AlertLogIDType ; 
    begin
      for i in ALERTLOG_BASE_ID to NumAlertLogIDsVar loop 
        CurParentID := AlertLogPtr(i).ParentID ; 
        if Name = AlertLogPtr(i).Name.all and 
          (CurParentID = ParentID or CurParentID = ALERTLOG_ID_NOT_ASSIGNED or ParentID = ALERTLOG_ID_NOT_ASSIGNED)
        then
          return i ;
        end if ; 
      end loop ;
      return ALERTLOG_ID_NOT_FOUND ; -- not found
    end function FindAlertLogID ;

    ------------------------------------------------------------
    impure function GetAlertLogID(Name : string ; ParentID : AlertLogIDType ; CreateHierarchy : Boolean) return AlertLogIDType is
    ------------------------------------------------------------
      variable ResultID : AlertLogIDType ; 
    begin
      ResultID := FindAlertLogID(Name, ParentID) ; 
      if ResultID /= ALERTLOG_ID_NOT_FOUND then 
        -- found it, set ParentID
        if AlertLogPtr(ResultID).ParentID = ALERTLOG_ID_NOT_ASSIGNED then 
          AlertLogPtr(ResultID).ParentID := ParentID ; 
        -- else -- do not update as ParentIDs are either same or input ParentID = ALERTLOG_ID_NOT_ASSIGNED
        end if ; 
      else
        ResultID := GetNextAlertLogID ; 
        NewAlertLogRec(ResultID, Name, ParentID) ;
        FoundAlertHierVar := TRUE ; 
        if CreateHierarchy then 
          FoundReportHierVar := TRUE ;
        end if ; 
      end if ; 
      return ResultID ; 
    end function GetAlertLogID ; 
    
    ------------------------------------------------------------
    impure function GetAlertLogParentID(AlertLogID : AlertLogIDType) return AlertLogIDType is
    ------------------------------------------------------------
    begin
      return AlertLogPtr(AlertLogID).ParentID ;
    end function GetAlertLogParentID ; 

    ------------------------------------------------------------
    ------------------------------------------------------------
    -- Accessor Methods
    ------------------------------------------------------------
    
    ------------------------------------------------------------
    procedure SetGlobalAlertEnable (A : boolean := TRUE) is
    ------------------------------------------------------------
    begin
      GlobalAlertEnabledVar := A ;
    end procedure SetGlobalAlertEnable ;
    
    ------------------------------------------------------------
    impure function GetGlobalAlertEnable return boolean is
    ------------------------------------------------------------
    begin
      return GlobalAlertEnabledVar ;
    end function GetGlobalAlertEnable ;
    
    ------------------------------------------------------------
    procedure IncAffirmCheckCount is
    ------------------------------------------------------------
    begin
      if GlobalAlertEnabledVar then 
          AffirmCheckCountVar := AffirmCheckCountVar + 1 ;
      end if ; 
    end procedure IncAffirmCheckCount ;

    ------------------------------------------------------------
    impure function GetAffirmCheckCount return natural is
    ------------------------------------------------------------
    begin
      return AffirmCheckCountVar ;
    end function GetAffirmCheckCount ;

--??    ------------------------------------------------------------
--??    procedure IncAffirmPassCount is
--??    ------------------------------------------------------------
--??    begin
--??      if GlobalAlertEnabledVar then 
--??          AffirmCheckCountVar  := AffirmCheckCountVar  + 1 ;
--??          AffirmPassedCountVar := AffirmPassedCountVar + 1 ;
--??      end if ; 
--??    end procedure IncAffirmPassCount ;
--??
--??    ------------------------------------------------------------
--??    impure function GetAffirmPassCount return natural is
--??    ------------------------------------------------------------
--??    begin
--??      return AffirmPassedCountVar ;
--??    end function GetAffirmPassCount ;
        
    ------------------------------------------------------------
    -- PT LOCAL
    procedure SetOneStopCount(
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ;
      Level        : AlertType ;
      Count        : integer 
    ) is 
    begin
      if AlertLogPtr(AlertLogID).AlertStopCount(Level) = integer'right then
        AlertLogPtr(AlertLogID).AlertStopCount(Level) := Count ;
      else
        AlertLogPtr(AlertLogID).AlertStopCount(Level) := 
          AlertLogPtr(AlertLogID).AlertStopCount(Level) + Count ;
      end if ; 
    end procedure SetOneStopCount ;    

    ------------------------------------------------------------
    procedure SetAlertStopCount(AlertLogID : AlertLogIDType ;  Level : AlertType ;  Count : integer) is
    ------------------------------------------------------------
    begin
      SetOneStopCount(AlertLogID, Level, Count) ; 
      if AlertLogID /= ALERTLOG_BASE_ID then 
        SetAlertStopCount(AlertLogPtr(AlertLogID).ParentID, Level, Count) ;
      end if ; 
    end procedure SetAlertStopCount ;    
    
    ------------------------------------------------------------
    impure function GetAlertStopCount(AlertLogID : AlertLogIDType ;  Level : AlertType) return integer is
    ------------------------------------------------------------
    begin
      return AlertLogPtr(AlertLogID).AlertStopCount(Level) ;
    end function GetAlertStopCount ; 
  
    ------------------------------------------------------------
    procedure SetAlertEnable(Level : AlertType ;  Enable : boolean) is 
    ------------------------------------------------------------
    begin
      for i in ALERTLOG_BASE_ID to NumAlertLogIDsVar loop
        AlertLogPtr(i).AlertEnabled(Level) := Enable ;
      end loop ;
    end procedure SetAlertEnable ;    

    ------------------------------------------------------------
    procedure SetAlertEnable(AlertLogID : AlertLogIDType ;  Level : AlertType ;  Enable : boolean ; DescendHierarchy : boolean := TRUE) is
    ------------------------------------------------------------
    begin
      AlertLogPtr(AlertLogID).AlertEnabled(Level) := Enable ;
      if DescendHierarchy then
        for i in AlertLogID+1 to NumAlertLogIDsVar loop 
          if AlertLogID = AlertLogPtr(i).ParentID then 
            SetAlertEnable(i, Level, Enable, DescendHierarchy) ; 
          end if ; 
        end loop ;
      end if ; 
    end procedure SetAlertEnable ;  

    ------------------------------------------------------------
    impure function GetAlertEnable(AlertLogID : AlertLogIDType ;  Level : AlertType) return boolean is
    ------------------------------------------------------------
    begin
      return AlertLogPtr(AlertLogID).AlertEnabled(Level) ;
    end function GetAlertEnable ; 

    ------------------------------------------------------------
    procedure SetLogEnable(Level : LogType ;  Enable : boolean) is 
    ------------------------------------------------------------
    begin
      for i in ALERTLOG_BASE_ID to NumAlertLogIDsVar loop
        AlertLogPtr(i).LogEnabled(Level) := Enable ;
      end loop ; 
    end procedure SetLogEnable ;    

    ------------------------------------------------------------
    procedure SetLogEnable(AlertLogID : AlertLogIDType ;  Level : LogType ;  Enable : boolean ; DescendHierarchy : boolean := TRUE) is
    ------------------------------------------------------------
    begin
      AlertLogPtr(AlertLogID).LogEnabled(Level) := Enable ;
      if DescendHierarchy then
        for i in AlertLogID+1 to NumAlertLogIDsVar loop 
          if AlertLogID = AlertLogPtr(i).ParentID then 
            SetLogEnable(i, Level, Enable, DescendHierarchy) ; 
          end if ; 
        end loop ;
      end if ; 
    end procedure SetLogEnable ;    

    ------------------------------------------------------------
    impure function GetLogEnable(AlertLogID : AlertLogIDType ;  Level : LogType) return boolean is
    ------------------------------------------------------------
    begin
      if Level = ALWAYS then 
        return TRUE ; 
      else
        return AlertLogPtr(AlertLogID).LogEnabled(Level) ; 
      end if ; 
    end function GetLogEnable ; 

    ------------------------------------------------------------
    -- PT Local
    procedure PrintLogLevels(
    ------------------------------------------------------------
      AlertLogID   : AlertLogIDType ;  
      Prefix       : string ;
      IndentAmount : integer 
    ) is 
      variable buf : line ; 
    begin
      write(buf, Prefix &  " "   & LeftJustify(AlertLogPtr(AlertLogID).Name.all, ReportJustifyAmountVar - IndentAmount)) ;        
      for i in LogIndexType loop 
        if AlertLogPtr(AlertLogID).LogEnabled(i) then 
--          write(buf, " "  & to_string(AlertLogPtr(AlertLogID).LogEnabled(i)) ) ;
          write(buf, " "  & to_string(i)) ;
        end if ; 
      end loop ; 
      WriteLine(buf) ; 
      for i in AlertLogID+1 to NumAlertLogIDsVar loop 
        if AlertLogID = AlertLogPtr(i).ParentID then 
          PrintLogLevels(
            AlertLogID    => i, 
            Prefix        => Prefix & "  ", 
            IndentAmount  => IndentAmount + 2
          ) ; 
        end if ; 
      end loop ; 
    end procedure PrintLogLevels ; 

    ------------------------------------------------------------
    procedure ReportLogEnables is
    ------------------------------------------------------------
    begin
      if ReportJustifyAmountVar <= 0 then
        SetJustify ; 
      end if ; 
      PrintLogLevels(ALERTLOG_BASE_ID, "", 0) ; 
    end procedure ReportLogEnables ; 

    ------------------------------------------------------------
    procedure SetAlertLogOptions (
    ------------------------------------------------------------
      FailOnWarning         : AlertLogOptionsType := OPT_INIT_PARM_DETECT ; 
      FailOnDisabledErrors  : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      ReportHierarchy       : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteAlertLevel       : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteAlertName        : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteAlertTime        : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteLogLevel         : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteLogName          : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      WriteLogTime          : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
      AlertPrefix           : string := OSVVM_STRING_INIT_PARM_DETECT ;
      LogPrefix             : string := OSVVM_STRING_INIT_PARM_DETECT ;
      ReportPrefix          : string := OSVVM_STRING_INIT_PARM_DETECT ;
      DoneName              : string := OSVVM_STRING_INIT_PARM_DETECT ;
      PassName              : string := OSVVM_STRING_INIT_PARM_DETECT ;
      FailName              : string := OSVVM_STRING_INIT_PARM_DETECT 
    ) is
    begin
      if FailOnWarning /= OPT_INIT_PARM_DETECT then
        FailOnWarningVar := IsEnabled(FailOnWarning) ;
      end if ;
      if FailOnDisabledErrors /= OPT_INIT_PARM_DETECT then
        FailOnDisabledErrorsVar := IsEnabled(FailOnDisabledErrors) ;
      end if ;
      if ReportHierarchy /= OPT_INIT_PARM_DETECT then
        ReportHierarchyVar := IsEnabled(ReportHierarchy) ;
      end if ;
      if WriteAlertLevel /= OPT_INIT_PARM_DETECT then
        WriteAlertLevelVar := IsEnabled(WriteAlertLevel) ;
      end if ;
      if WriteAlertName /= OPT_INIT_PARM_DETECT then
        WriteAlertNameVar := IsEnabled(WriteAlertName) ;
      end if ;
      if WriteAlertTime /= OPT_INIT_PARM_DETECT then
        WriteAlertTimeVar := IsEnabled(WriteAlertTime) ;
      end if ;
      if WriteLogLevel /= OPT_INIT_PARM_DETECT then
        WriteLogLevelVar := IsEnabled(WriteLogLevel) ;
      end if ;
      if WriteLogName /= OPT_INIT_PARM_DETECT then
        WriteLogNameVar := IsEnabled(WriteLogName) ;
      end if ;
      if WriteLogTime /= OPT_INIT_PARM_DETECT then
        WriteLogTimeVar := IsEnabled(WriteLogTime) ;
      end if ;
      if AlertPrefix /= OSVVM_STRING_INIT_PARM_DETECT then
        AlertPrefixVar.Set(AlertPrefix) ; 
      end if ;
      if LogPrefix /= OSVVM_STRING_INIT_PARM_DETECT then
        LogPrefixVar.Set(LogPrefix) ; 
      end if ;
      if ReportPrefix /= OSVVM_STRING_INIT_PARM_DETECT then
        ReportPrefixVar.Set(ReportPrefix) ; 
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
    end procedure SetAlertLogOptions ;
    
    ------------------------------------------------------------
    procedure ReportAlertLogOptions is
    ------------------------------------------------------------
      variable buf : line ; 
    begin
      -- Boolean Values
      swrite(buf, "ReportAlertLogOptions" & LF ) ;
      swrite(buf, "---------------------" & LF ) ;
      swrite(buf, "FailOnWarningVar:        " & to_string(FailOnWarningVar        ) & LF ) ;
      swrite(buf, "FailOnDisabledErrorsVar: " & to_string(FailOnDisabledErrorsVar ) & LF ) ;
      swrite(buf, "ReportHierarchyVar:      " & to_string(ReportHierarchyVar      ) & LF ) ;
      swrite(buf, "FoundReportHierVar:      " & to_string(FoundReportHierVar      ) & LF ) ; -- Not set by user
      swrite(buf, "FoundAlertHierVar:       " & to_string(FoundAlertHierVar       ) & LF ) ; -- Not set by user
      swrite(buf, "WriteAlertLevelVar:      " & to_string(WriteAlertLevelVar      ) & LF ) ;
      swrite(buf, "WriteAlertNameVar:       " & to_string(WriteAlertNameVar       ) & LF ) ;
      swrite(buf, "WriteAlertTimeVar:       " & to_string(WriteAlertTimeVar       ) & LF ) ;
      swrite(buf, "WriteLogLevelVar:        " & to_string(WriteLogLevelVar        ) & LF ) ;
      swrite(buf, "WriteLogNameVar:         " & to_string(WriteLogNameVar         ) & LF ) ;
      swrite(buf, "WriteLogTimeVar:         " & to_string(WriteLogTimeVar         ) & LF ) ;
      
      -- String
      swrite(buf, "AlertPrefixVar:          " & string'(AlertPrefixVar.Get(OSVVM_DEFAULT_ALERT_PREFIX))  & LF ) ;
      swrite(buf, "LogPrefixVar:            " & string'(LogPrefixVar.Get(OSVVM_DEFAULT_LOG_PREFIX))      & LF ) ;
      swrite(buf, "ReportPrefixVar:         " & ResolveOsvvmWritePrefix(ReportPrefixVar.GetOpt) & LF ) ; 
      swrite(buf, "DoneNameVar:             " & ResolveOsvvmDoneName(DoneNameVar.GetOpt)        & LF ) ;
      swrite(buf, "PassNameVar:             " & ResolveOsvvmPassName(PassNameVar.GetOpt)        & LF ) ;
      swrite(buf, "FailNameVar:             " & ResolveOsvvmFailName(FailNameVar.GetOpt)        & LF ) ;
      writeline(buf) ; 
    end procedure ReportAlertLogOptions ; 
    
    ------------------------------------------------------------
    impure function GetAlertLogFailOnWarning        return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(FailOnWarningVar) ; 
    end function GetAlertLogFailOnWarning ;
    
    ------------------------------------------------------------
    impure function GetAlertLogFailOnDisabledErrors return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(FailOnDisabledErrorsVar) ; 
    end function GetAlertLogFailOnDisabledErrors ;
    
    ------------------------------------------------------------
    impure function GetAlertLogReportHierarchy      return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(ReportHierarchyVar) ; 
    end function GetAlertLogReportHierarchy ;
    
    ------------------------------------------------------------
    impure function GetAlertLogFoundReportHier       return boolean is
    ------------------------------------------------------------
    begin
      return FoundReportHierVar ; 
    end function GetAlertLogFoundReportHier ;

    ------------------------------------------------------------
    impure function GetAlertLogFoundAlertHier       return boolean is
    ------------------------------------------------------------
    begin
      return FoundAlertHierVar ; 
    end function GetAlertLogFoundAlertHier ;
    
    ------------------------------------------------------------
    impure function GetAlertLogWriteAlertLevel      return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(WriteAlertLevelVar) ; 
    end function GetAlertLogWriteAlertLevel ;
    
    ------------------------------------------------------------
    impure function GetAlertLogWriteAlertName       return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(WriteAlertNameVar) ; 
    end function GetAlertLogWriteAlertName ;
    
    ------------------------------------------------------------
    impure function GetAlertLogWriteAlertTime       return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(WriteAlertTimeVar) ; 
    end function GetAlertLogWriteAlertTime ;
    
    ------------------------------------------------------------
    impure function GetAlertLogWriteLogLevel        return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(WriteLogLevelVar) ; 
    end function GetAlertLogWriteLogLevel ;
    
    ------------------------------------------------------------
    impure function GetAlertLogWriteLogName         return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(WriteLogNameVar) ; 
    end function GetAlertLogWriteLogName ;
    
    ------------------------------------------------------------
    impure function GetAlertLogWriteLogTime         return AlertLogOptionsType is
    ------------------------------------------------------------
    begin
      return to_OsvvmOptionsType(WriteLogTimeVar) ; 
    end function GetAlertLogWriteLogTime ;

    ------------------------------------------------------------
    impure function GetAlertLogAlertPrefix          return string is
    ------------------------------------------------------------
    begin
      return AlertPrefixVar.Get(OSVVM_DEFAULT_ALERT_PREFIX) ; 
    end function GetAlertLogAlertPrefix ;
    
    ------------------------------------------------------------
    impure function GetAlertLogLogPrefix            return string is
    ------------------------------------------------------------
    begin
      return LogPrefixVar.Get(OSVVM_DEFAULT_LOG_PREFIX) ; 
    end function GetAlertLogLogPrefix ;

    ------------------------------------------------------------
    impure function GetAlertLogReportPrefix return string is
    ------------------------------------------------------------
    begin
      return ResolveOsvvmWritePrefix(ReportPrefixVar.GetOpt) ; 
    end function GetAlertLogReportPrefix ;

    ------------------------------------------------------------
    impure function GetAlertLogDoneName return string is
    ------------------------------------------------------------
    begin
      return ResolveOsvvmDoneName(DoneNameVar.GetOpt) ; 
    end function GetAlertLogDoneName ;

    ------------------------------------------------------------
    impure function GetAlertLogPassName return string is
    ------------------------------------------------------------
    begin
      return ResolveOsvvmPassName(PassNameVar.GetOpt) ; 
    end function GetAlertLogPassName ;

    ------------------------------------------------------------
    impure function GetAlertLogFailName return string is
    ------------------------------------------------------------
    begin
      return ResolveOsvvmFailName(FailNameVar.GetOpt) ; 
    end function GetAlertLogFailName ;
    
  end protected body AlertLogStructPType ;



  shared variable AlertLogStruct : AlertLogStructPType ; 

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
    
  ------------------------------------------------------------
  procedure Alert( 
  ------------------------------------------------------------
    AlertLogID   : AlertLogIDType ; 
    Message      : string  ; 
    Level        : AlertType := ERROR
  ) is
  begin
    AlertLogStruct.Alert(AlertLogID, Message, Level) ; 
  end procedure alert ; 

  ------------------------------------------------------------
  procedure Alert( Message : string ; Level : AlertType := ERROR ) is 
  ------------------------------------------------------------
  begin
    AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message, Level) ; 
  end procedure alert ; 
  
  ------------------------------------------------------------
  procedure IncAlertCount( 
  ------------------------------------------------------------
    AlertLogID   : AlertLogIDType ; 
    Level        : AlertType := ERROR
  ) is
  begin
    AlertLogStruct.IncAlertCount(AlertLogID, Level) ; 
  end procedure IncAlertCount ; 

  ------------------------------------------------------------
  procedure IncAlertCount( Level : AlertType := ERROR ) is 
  ------------------------------------------------------------
  begin
    AlertLogStruct.IncAlertCount(ALERT_DEFAULT_ID, Level) ; 
  end procedure IncAlertCount ; 
  

  ------------------------------------------------------------
  procedure AlertIf( AlertLogID  : AlertLogIDType ; condition : boolean ; Message : string ; Level : AlertType := ERROR ) is
  ------------------------------------------------------------
  begin
    if condition then
      AlertLogStruct.Alert(AlertLogID , Message, Level) ; 
    end if ; 
  end procedure AlertIf ; 

  ------------------------------------------------------------
  -- deprecated
  procedure AlertIf( condition : boolean ; AlertLogID  : AlertLogIDType ; Message : string ; Level : AlertType := ERROR ) is
  ------------------------------------------------------------
  begin
    AlertIf( AlertLogID, condition, Message, Level) ; 
  end procedure AlertIf ; 

  ------------------------------------------------------------
  procedure AlertIf( condition : boolean ; Message : string ; Level : AlertType := ERROR ) is
  ------------------------------------------------------------
  begin
    if condition then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID , Message, Level) ; 
    end if ; 
  end procedure AlertIf ;  
  
  ------------------------------------------------------------
  -- useful with exit conditions in a loop:  exit when alert( not ReadValid, failure, "Read Failed") ; 
  impure function  AlertIf( AlertLogID  : AlertLogIDType ; condition : boolean ; Message : string ; Level : AlertType := ERROR ) return boolean is
  ------------------------------------------------------------
  begin
    if condition then
      AlertLogStruct.Alert(AlertLogID , Message, Level) ; 
    end if ; 
    return condition ; 
  end function AlertIf ;  
  
  ------------------------------------------------------------
  -- deprecated
  impure function  AlertIf( condition : boolean ; AlertLogID  : AlertLogIDType ; Message : string ; Level : AlertType := ERROR ) return boolean is
  ------------------------------------------------------------
  begin
    return AlertIf( AlertLogID, condition, Message, Level) ; 
  end function AlertIf ;  
  
  ------------------------------------------------------------
  impure function  AlertIf( condition : boolean ; Message : string ; Level : AlertType := ERROR ) return boolean is
  ------------------------------------------------------------
  begin
    if condition then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message, Level) ; 
    end if ; 
    return condition ; 
  end function AlertIf ;   
  
  ------------------------------------------------------------
  procedure AlertIfNot( AlertLogID  : AlertLogIDType ; condition : boolean ; Message : string ; Level : AlertType := ERROR ) is
  ------------------------------------------------------------
  begin
    if not condition then
      AlertLogStruct.Alert(AlertLogID, Message, Level) ; 
    end if ; 
  end procedure AlertIfNot ; 

  ------------------------------------------------------------
  -- deprecated
  procedure AlertIfNot( condition : boolean ; AlertLogID  : AlertLogIDType ; Message : string ; Level : AlertType := ERROR ) is
  ------------------------------------------------------------
  begin
    AlertIfNot( AlertLogID, condition, Message, Level) ; 
  end procedure AlertIfNot ; 

  ------------------------------------------------------------
  procedure AlertIfNot( condition : boolean ; Message : string ; Level : AlertType := ERROR ) is
  ------------------------------------------------------------
  begin
    if not condition then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message, Level) ; 
    end if ; 
  end procedure AlertIfNot ;  
  
  ------------------------------------------------------------
  -- useful with exit conditions in a loop:  exit when alert( not ReadValid, failure, "Read Failed") ; 
  impure function  AlertIfNot( AlertLogID  : AlertLogIDType ; condition : boolean ; Message : string ; Level : AlertType := ERROR ) return boolean is
  ------------------------------------------------------------
  begin
    if not condition then
      AlertLogStruct.Alert(AlertLogID, Message, Level) ; 
    end if ; 
    return not condition ; 
  end function AlertIfNot ;  
  
  ------------------------------------------------------------
  -- deprecated
  impure function  AlertIfNot( condition : boolean ; AlertLogID  : AlertLogIDType ; Message : string ; Level : AlertType := ERROR ) return boolean is
  ------------------------------------------------------------
  begin
    return AlertIfNot( AlertLogID, condition, Message, Level) ; 
  end function AlertIfNot ;  
  
  ------------------------------------------------------------
  impure function  AlertIfNot( condition : boolean ; Message : string ; Level : AlertType := ERROR ) return boolean is
  ------------------------------------------------------------
  begin
    if not condition then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message, Level) ; 
    end if ; 
    return not condition ; 
  end function AlertIfNot ;   
    
  -- With AlertLogID
  ------------------------------------------------------------
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ; L, R : std_logic ;        Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ; L, R : std_logic_vector ; Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ; L, R : unsigned ;         Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ; L, R : signed ;           Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ; L, R : integer ;          Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L = R then
      AlertLogStruct.Alert(AlertLogID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ; L, R : real ;             Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L = R then
      AlertLogStruct.Alert(AlertLogID, Message & " L = R,  L = " & to_string(L, 4) & "   R = " & to_string(R, 4), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ; L, R : character ;        Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L = R then
      AlertLogStruct.Alert(AlertLogID, Message & " L = R,  L = " & L & "   R = " & R, Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( AlertLogID : AlertLogIDType ; L, R : string ;           Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L = R then
      AlertLogStruct.Alert(AlertLogID, Message & " L = R,  L = " & L & "   R = " & R, Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 

  -- Without AlertLogID
  ------------------------------------------------------------
  procedure AlertIfEqual( L, R : std_logic ;        Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( L, R : std_logic_vector ; Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( L, R : unsigned ;         Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( L, R : signed ;           Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( L, R : integer ;          Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L = R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L = R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( L, R : real ;             Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L = R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L = R,  L = " & to_string(L, 4) & "   R = " & to_string(R, 4), Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( L, R : character ;        Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L = R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L = R,  L = " & L & "   R = " & R, Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfEqual( L, R : string ;           Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L = R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L = R,  L = " & L & "   R = " & R, Level) ; 
    end if ; 
  end procedure AlertIfEqual ; 

  -- With AlertLogID
  ------------------------------------------------------------
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ; L, R : std_logic ;        Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?/= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ; L, R : std_logic_vector ; Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?/= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ; L, R : unsigned ;         Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?/= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ; L, R : signed ;           Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?/= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ; L, R : integer ;          Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L /= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ; L, R : real ;             Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L /= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L /= R,  L = " & to_string(L, 4) & "   R = " & to_string(R, 4), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ; L, R : character ;        Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L /= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L /= R,  L = " & L & "   R = " & R, Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( AlertLogID : AlertLogIDType ; L, R : string ;           Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L /= R then
      AlertLogStruct.Alert(AlertLogID, Message & " L /= R,  L = " & L & "   R = " & R, Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 

  -- Without AlertLogID
  ------------------------------------------------------------
  procedure AlertIfNotEqual( L, R : std_logic ;        Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?/= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( L, R : std_logic_vector ; Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?/= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( L, R : unsigned ;         Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?/= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( L, R : signed ;           Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L ?/= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( L, R : integer ;          Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L /= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L /= R,  L = " & to_string(L) & "   R = " & to_string(R), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( L, R : real ;             Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L /= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L /= R,  L = " & to_string(L, 4) & "   R = " & to_string(R, 4), Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( L, R : character ;        Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L /= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L /= R,  L = " & L & "   R = " & R, Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 
  
  ------------------------------------------------------------
  procedure AlertIfNotEqual( L, R : string ;           Message : string ; Level : AlertType := ERROR )  is
  ------------------------------------------------------------
  begin
    if L /= R then
      AlertLogStruct.Alert(ALERT_DEFAULT_ID, Message & " L /= R,  L = " & L & "   R = " & R, Level) ; 
    end if ; 
  end procedure AlertIfNotEqual ; 

  ------------------------------------------------------------
  procedure AlertIfDiff (AlertLogID : AlertLogIDType ; Name1, Name2 : string; Message : string := "" ; Level : AlertType := ERROR ) is 
  -- Open files and call AlertIfDiff[text, ...]    
  ------------------------------------------------------------
    file FileID1, FileID2 : text ; 
    variable status1, status2 : file_open_status ; 
  begin
    file_open(status1, FileID1, Name1, READ_MODE) ; 
    file_open(status2, FileID2, Name2, READ_MODE) ; 
    if status1 = OPEN_OK and status2 = OPEN_OK then 
      AlertIfDiff (AlertLogID, FileID1, FileID2, Message & " " & Name1 & " /= " & Name2 & ", ", Level) ; 
    else 
      if status1 /= OPEN_OK then
        AlertLogStruct.Alert(AlertLogID , Message & " File, " & Name1 & ", did not open", Level) ; 
      end if ; 
      if status2 /= OPEN_OK then
        AlertLogStruct.Alert(AlertLogID , Message & " File, " & Name2 & ", did not open", Level) ; 
      end if ; 
    end if; 
  end procedure AlertIfDiff ; 
  
  ------------------------------------------------------------
  procedure AlertIfDiff (Name1, Name2 : string; Message : string := "" ; Level : AlertType := ERROR ) is 
  ------------------------------------------------------------
  begin
    AlertIfDiff (ALERT_DEFAULT_ID, Name1, Name2, Message, Level) ; 
  end procedure AlertIfDiff ;     
  
  ------------------------------------------------------------
  procedure AlertIfDiff (AlertLogID : AlertLogIDType ; file File1, File2 : text; Message : string := "" ; Level : AlertType := ERROR ) is 
  -- Simple diff.    
  ------------------------------------------------------------
    variable Buf1, Buf2 : line ; 
    variable File1Done, File2Done : boolean ; 
    variable LineCount : integer := 0 ; 
  begin
    ReadLoop : loop
      File1Done := EndFile(File1) ; 
      File2Done := EndFile(File2) ;
      exit ReadLoop when File1Done or File2Done ;

      ReadLine(File1, Buf1) ; 
      ReadLine(File2, Buf2) ;
      LineCount := LineCount + 1 ; 

      if Buf1.all /= Buf2.all then 
        AlertLogStruct.Alert(AlertLogID , Message & " File miscompare on line " & to_string(LineCount), Level) ; 
        exit ReadLoop ; 
      end if ;     
    end loop ReadLoop ;
    if File1Done /= File2Done then
      if not File1Done then
        AlertLogStruct.Alert(AlertLogID , Message & " File1 longer than File2 " & to_string(LineCount), Level) ; 
      end if ;
      if not File2Done then
        AlertLogStruct.Alert(AlertLogID , Message & " File2 longer than File1 " & to_string(LineCount), Level) ; 
      end if ;
    end if; 
  end procedure AlertIfDiff ;   
  
  ------------------------------------------------------------
  procedure AlertIfDiff (file File1, File2 : text; Message : string := "" ; Level : AlertType := ERROR ) is 
  ------------------------------------------------------------
  begin
    AlertIfDiff (ALERT_DEFAULT_ID, File1, File2, Message, Level) ; 
  end procedure AlertIfDiff ;   

  ------------------------------------------------------------
  procedure AffirmIf(
  ------------------------------------------------------------
    AlertLogID   : AlertLogIDType ;
    condition    : boolean ;
    Message      : string ;
    LogLevel     : LogType := PASSED ;
    AlertLevel   : AlertType := ERROR
  ) is
  begin
    AlertLogStruct.IncAffirmCheckCount ; -- increment check count
    if condition then
      -- passed
      AlertLogStruct.Log(AlertLogID, Message, LogLevel) ; -- call log
--      AlertLogStruct.IncAffirmPassCount ; -- increment pass & check count
    else
      AlertLogStruct.Alert(AlertLogID, Message, AlertLevel) ; -- signal failure
    end if ;
  end procedure AffirmIf ;

  ------------------------------------------------------------
  procedure AffirmIf(condition : boolean ; Message : string ;  LogLevel : LogType := PASSED ; AlertLevel : AlertType := ERROR) is
  ------------------------------------------------------------
  begin
    AffirmIf(ALERT_DEFAULT_ID, condition, Message, LogLevel, AlertLevel) ; 
  end procedure AffirmIf;  

  ------------------------------------------------------------
  procedure SetAlertLogJustify is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetJustify ;
  end procedure SetAlertLogJustify ; 
  
  ------------------------------------------------------------
  procedure ReportAlerts ( Name : String ; AlertCount : AlertCountType ) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.ReportAlerts(Name, AlertCount) ; 
  end procedure ReportAlerts ; 

  ------------------------------------------------------------
  procedure ReportAlerts ( Name : string := OSVVM_STRING_INIT_PARM_DETECT ; AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID ; ExternalErrors : AlertCountType := (others => 0) ) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.ReportAlerts(Name, AlertLogID, ExternalErrors, TRUE) ; 
  end procedure ReportAlerts ; 

  ------------------------------------------------------------
  procedure ReportNonZeroAlerts ( Name : string := OSVVM_STRING_INIT_PARM_DETECT ; AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID ; ExternalErrors : AlertCountType := (others => 0) ) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.ReportAlerts(Name, AlertLogID, ExternalErrors, FALSE) ; 
  end procedure ReportNonZeroAlerts ; 

  ------------------------------------------------------------
  procedure ClearAlerts is
  ------------------------------------------------------------
  begin
    AlertLogStruct.ClearAlerts ; 
  end procedure ClearAlerts ; 

  ------------------------------------------------------------
  function "ABS" (L : AlertCountType) return AlertCountType is
  ------------------------------------------------------------
    variable Result : AlertCountType ; 
  begin
    Result(FAILURE) := ABS( L(FAILURE) ) ; 
    Result(ERROR)   := ABS( L(ERROR) ) ; 
    Result(WARNING) := ABS( L(WARNING) ); 
    return Result ; 
  end function "ABS" ;

  ------------------------------------------------------------
  function "+" (L, R : AlertCountType) return AlertCountType is
  ------------------------------------------------------------
    variable Result : AlertCountType ; 
  begin
    Result(FAILURE) := L(FAILURE) + R(FAILURE) ; 
    Result(ERROR)   := L(ERROR)   + R(ERROR) ; 
    Result(WARNING) := L(WARNING) + R(WARNING) ; 
    return Result ; 
  end function "+" ;

  ------------------------------------------------------------
  function "-" (L, R : AlertCountType) return AlertCountType is
  ------------------------------------------------------------
    variable Result : AlertCountType ; 
  begin
    Result(FAILURE) := L(FAILURE) - R(FAILURE) ; 
    Result(ERROR)   := L(ERROR)   - R(ERROR) ; 
    Result(WARNING) := L(WARNING) - R(WARNING) ; 
    return Result ; 
  end function "-" ;

  ------------------------------------------------------------
  function "-" (R : AlertCountType) return AlertCountType is
  ------------------------------------------------------------
    variable Result : AlertCountType ; 
  begin
    Result(FAILURE) := - R(FAILURE) ; 
    Result(ERROR)   := - R(ERROR) ; 
    Result(WARNING) := - R(WARNING) ; 
    return Result ; 
  end function "-" ;
  
  ------------------------------------------------------------
  impure function SumAlertCount(AlertCount: AlertCountType) return integer is
  ------------------------------------------------------------
  begin
    -- Using ABS ensures correct expected error handling.  
    return abs(AlertCount(FAILURE)) + abs(AlertCount(ERROR)) + abs(AlertCount(WARNING)) ; 
  end function SumAlertCount ; 

  ------------------------------------------------------------
  impure function GetAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return AlertCountType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertCount(AlertLogID) ; 
  end function GetAlertCount ; 
    
  ------------------------------------------------------------
  impure function GetAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return integer is
  ------------------------------------------------------------
  begin
    return SumAlertCount(AlertLogStruct.GetAlertCount(AlertLogID)) ; 
  end function GetAlertCount ; 

  ------------------------------------------------------------
  impure function GetEnabledAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return AlertCountType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetEnabledAlertCount(AlertLogID) ; 
  end function GetEnabledAlertCount ; 
    
  ------------------------------------------------------------
  impure function GetEnabledAlertCount(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return integer is
  ------------------------------------------------------------
  begin
    return SumAlertCount(AlertLogStruct.GetEnabledAlertCount(AlertLogID)) ; 
  end function GetEnabledAlertCount ; 

  ------------------------------------------------------------
  impure function GetDisabledAlertCount return AlertCountType is
  ------------------------------------------------------------
   begin
    return AlertLogStruct.GetDisabledAlertCount ; 
  end function GetDisabledAlertCount ; 

  ------------------------------------------------------------
  impure function GetDisabledAlertCount return integer is
  ------------------------------------------------------------
   begin
    return SumAlertCount(AlertLogStruct.GetDisabledAlertCount) ; 
  end function GetDisabledAlertCount ; 

  ------------------------------------------------------------
  impure function GetDisabledAlertCount(AlertLogID: AlertLogIDType) return AlertCountType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetDisabledAlertCount(AlertLogID) ; 
  end function GetDisabledAlertCount ; 
  
  ------------------------------------------------------------
  impure function GetDisabledAlertCount(AlertLogID: AlertLogIDType) return integer is
  ------------------------------------------------------------
  begin
    return SumAlertCount(AlertLogStruct.GetDisabledAlertCount(AlertLogID)) ; 
  end function GetDisabledAlertCount ; 
  
  ------------------------------------------------------------
  procedure Log( 
    AlertLogID   : AlertLogIDType ; 
    Message      : string ;
    Level        : LogType := ALWAYS ;
    Enable       : boolean := FALSE    -- override internal enable
  ) is
  begin
    AlertLogStruct.Log(AlertLogID, Message, Level, Enable) ; 
  end procedure log ;
  
  ------------------------------------------------------------
  procedure Log( Message : string ; Level : LogType := ALWAYS ; Enable : boolean := FALSE) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.Log(LOG_DEFAULT_ID, Message, Level, Enable) ; 
  end procedure log ;
    
 ------------------------------------------------------------
  procedure SetAlertLogName(Name : string ) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetAlertLogName(Name) ;
  end procedure SetAlertLogName ;
  
  ------------------------------------------------------------
  impure function GetAlertLogName(AlertLogID : AlertLogIDType := ALERTLOG_BASE_ID) return string is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogName(AlertLogID) ; 
  end GetAlertLogName ;
  
  ------------------------------------------------------------
  procedure DeallocateAlertLogStruct is
  ------------------------------------------------------------
  begin
    AlertLogStruct.Deallocate ;
  end procedure DeallocateAlertLogStruct ; 

  ------------------------------------------------------------
  procedure InitializeAlertLogStruct is
  ------------------------------------------------------------
  begin
    AlertLogStruct.Initialize ;
  end procedure InitializeAlertLogStruct ;
  
  ------------------------------------------------------------
  impure function FindAlertLogID(Name : string ) return AlertLogIDType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.FindAlertLogID(Name) ;
  end function FindAlertLogID ;
  
  ------------------------------------------------------------
  impure function FindAlertLogID(Name : string ; ParentID : AlertLogIDType) return AlertLogIDType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.FindAlertLogID(Name, ParentID) ;
  end function FindAlertLogID ;
  
  ------------------------------------------------------------
  impure function GetAlertLogID(Name : string ; ParentID : AlertLogIDType := ALERTLOG_BASE_ID ; CreateHierarchy : Boolean := TRUE) return AlertLogIDType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogID(Name, ParentID, CreateHierarchy ) ;
  end function GetAlertLogID ;
  
  ------------------------------------------------------------
  impure function GetAlertLogParentID(AlertLogID : AlertLogIDType) return AlertLogIDType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogParentID(AlertLogID) ;
  end function GetAlertLogParentID ; 

  ------------------------------------------------------------
  procedure SetGlobalAlertEnable (A : boolean := TRUE) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetGlobalAlertEnable(A) ;
  end procedure SetGlobalAlertEnable ;
  
  ------------------------------------------------------------
  -- Set using constant.  Set before code runs.
  impure function SetGlobalAlertEnable (A : boolean := TRUE) return boolean is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetGlobalAlertEnable(A) ;
    return A ;  
  end function SetGlobalAlertEnable ;

  ------------------------------------------------------------
  impure function GetGlobalAlertEnable return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetGlobalAlertEnable ;
  end function GetGlobalAlertEnable ;
  
  ------------------------------------------------------------
  procedure IncAffirmCheckCount is 
  ------------------------------------------------------------
  begin
    AlertLogStruct.IncAffirmCheckCount ;
  end procedure IncAffirmCheckCount ;

  ------------------------------------------------------------
  impure function GetAffirmCheckCount return natural is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAffirmCheckCount ;
  end function GetAffirmCheckCount ;

--??  ------------------------------------------------------------
--??  procedure IncAffirmPassCount is
--??  ------------------------------------------------------------
--??  begin
--??    AlertLogStruct.IncAffirmPassCount ;
--??  end procedure IncAffirmPassCount ;
--??
--??  ------------------------------------------------------------
--??  impure function GetAffirmPassCount return natural is
--??  ------------------------------------------------------------
--??  begin
--??    return AlertLogStruct.GetAffirmPassCount ;
--??  end function GetAffirmPassCount ;
        
  ------------------------------------------------------------
  procedure SetAlertStopCount(AlertLogID : AlertLogIDType ;  Level : AlertType ;  Count : integer) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetAlertStopCount(AlertLogID, Level, Count) ;
  end procedure SetAlertStopCount ;    

  ------------------------------------------------------------
  procedure SetAlertStopCount(Level : AlertType ;  Count : integer) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetAlertStopCount(ALERTLOG_BASE_ID, Level, Count) ;
  end procedure SetAlertStopCount ;    

  ------------------------------------------------------------
  impure function GetAlertStopCount(AlertLogID : AlertLogIDType ;  Level : AlertType) return integer is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertStopCount(AlertLogID, Level) ;
  end function GetAlertStopCount ; 
  
  ------------------------------------------------------------
  impure function GetAlertStopCount(Level : AlertType) return integer is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertStopCount(ALERTLOG_BASE_ID, Level) ;
  end function GetAlertStopCount ; 
  
  ------------------------------------------------------------
  procedure SetAlertEnable(Level : AlertType ;  Enable : boolean) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetAlertEnable(Level, Enable) ;
  end procedure SetAlertEnable ;    

  ------------------------------------------------------------
  procedure SetAlertEnable(AlertLogID : AlertLogIDType ;  Level : AlertType ;  Enable : boolean ; DescendHierarchy : boolean := TRUE) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetAlertEnable(AlertLogID, Level, Enable, DescendHierarchy) ;
  end procedure SetAlertEnable ;    
  
  ------------------------------------------------------------
  impure function GetAlertEnable(AlertLogID : AlertLogIDType ;  Level : AlertType) return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertEnable(AlertLogID, Level) ;
  end function GetAlertEnable ; 

  ------------------------------------------------------------
  impure function GetAlertEnable(Level : AlertType) return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertEnable(ALERT_DEFAULT_ID, Level) ;
  end function GetAlertEnable ; 

  ------------------------------------------------------------
  procedure SetLogEnable(Level : LogType ;  Enable : boolean) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetLogEnable(Level, Enable) ;
  end procedure SetLogEnable ;    

  ------------------------------------------------------------
  procedure SetLogEnable(AlertLogID : AlertLogIDType ;  Level : LogType ;  Enable : boolean ; DescendHierarchy : boolean := TRUE) is
  ------------------------------------------------------------
  begin
    AlertLogStruct.SetLogEnable(AlertLogID, Level, Enable, DescendHierarchy) ;
  end procedure SetLogEnable ;  

  ------------------------------------------------------------
  impure function GetLogEnable(AlertLogID : AlertLogIDType ;  Level : LogType) return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetLogEnable(AlertLogID, Level) ;
  end function GetLogEnable ; 

  ------------------------------------------------------------
  impure function GetLogEnable(Level : LogType) return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetLogEnable(LOG_DEFAULT_ID, Level) ;
  end function GetLogEnable ; 

  ------------------------------------------------------------
  impure function IsLoggingEnabled(AlertLogID : AlertLogIDType ; Level : LogType) return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetLogEnable(AlertLogID, Level) ;
  end function IsLoggingEnabled ; 
    
  ------------------------------------------------------------
  impure function IsLoggingEnabled(Level : LogType) return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetLogEnable(LOG_DEFAULT_ID, Level) ;
  end function IsLoggingEnabled ; 
    
  ------------------------------------------------------------
  procedure ReportLogEnables is
  ------------------------------------------------------------
  begin
    AlertLogStruct.ReportLogEnables ; 
  end ReportLogEnables ;
  
  ------------------------------------------------------------
  procedure SetAlertLogOptions (
  ------------------------------------------------------------
    FailOnWarning         : AlertLogOptionsType := OPT_INIT_PARM_DETECT ; 
    FailOnDisabledErrors  : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    ReportHierarchy       : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteAlertLevel       : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteAlertName        : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteAlertTime        : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteLogLevel         : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteLogName          : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    WriteLogTime          : AlertLogOptionsType := OPT_INIT_PARM_DETECT ;
    AlertPrefix           : string := OSVVM_STRING_INIT_PARM_DETECT ;
    LogPrefix             : string := OSVVM_STRING_INIT_PARM_DETECT ;
    ReportPrefix          : string := OSVVM_STRING_INIT_PARM_DETECT ;
    DoneName              : string := OSVVM_STRING_INIT_PARM_DETECT ;
    PassName              : string := OSVVM_STRING_INIT_PARM_DETECT ;
    FailName              : string := OSVVM_STRING_INIT_PARM_DETECT 
  ) is
  begin
    AlertLogStruct.SetAlertLogOptions (
      FailOnWarning         => FailOnWarning       ,
      FailOnDisabledErrors  => FailOnDisabledErrors,
      ReportHierarchy       => ReportHierarchy     ,
      WriteAlertLevel       => WriteAlertLevel     ,
      WriteAlertName        => WriteAlertName     ,
      WriteAlertTime        => WriteAlertTime      ,
      WriteLogLevel         => WriteLogLevel       ,
      WriteLogName          => WriteLogName       ,
      WriteLogTime          => WriteLogTime        ,
      AlertPrefix           => AlertPrefix         ,
      LogPrefix             => LogPrefix           ,
      ReportPrefix          => ReportPrefix        ,
      DoneName              => DoneName            ,
      PassName              => PassName            ,
      FailName              => FailName           
    ); 
  end procedure SetAlertLogOptions ;
  
  ------------------------------------------------------------
  procedure ReportAlertLogOptions is
  ------------------------------------------------------------
  begin
    AlertLogStruct.ReportAlertLogOptions ;
  end procedure ReportAlertLogOptions ; 
  
  ------------------------------------------------------------
  impure function GetAlertLogFailOnWarning        return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogFailOnWarning ; 
  end function GetAlertLogFailOnWarning ;
  
  ------------------------------------------------------------
  impure function GetAlertLogFailOnDisabledErrors return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogFailOnDisabledErrors ; 
  end function GetAlertLogFailOnDisabledErrors ;
  
  ------------------------------------------------------------
  impure function GetAlertLogReportHierarchy      return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogReportHierarchy ; 
  end function GetAlertLogReportHierarchy ;
  
  ------------------------------------------------------------
  impure function GetAlertLogFoundReportHier       return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogFoundReportHier ; 
  end function GetAlertLogFoundReportHier ;
  
  ------------------------------------------------------------
  impure function GetAlertLogFoundAlertHier       return boolean is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogFoundAlertHier ; 
  end function GetAlertLogFoundAlertHier ;

  ------------------------------------------------------------
  impure function GetAlertLogWriteAlertLevel      return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogWriteAlertLevel ; 
  end function GetAlertLogWriteAlertLevel ;
  
  ------------------------------------------------------------
  impure function GetAlertLogWriteAlertName       return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogWriteAlertName ; 
  end function GetAlertLogWriteAlertName ;
  
  ------------------------------------------------------------
  impure function GetAlertLogWriteAlertTime       return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogWriteAlertTime ; 
  end function GetAlertLogWriteAlertTime ;
  
  ------------------------------------------------------------
  impure function GetAlertLogWriteLogLevel        return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogWriteLogLevel ; 
  end function GetAlertLogWriteLogLevel ;
  
  ------------------------------------------------------------
  impure function GetAlertLogWriteLogName         return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogWriteLogName ; 
  end function GetAlertLogWriteLogName ;
  
  ------------------------------------------------------------
  impure function GetAlertLogWriteLogTime         return AlertLogOptionsType is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogWriteLogTime ; 
  end function GetAlertLogWriteLogTime ;

  ------------------------------------------------------------
  impure function GetAlertLogAlertPrefix          return string is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogAlertPrefix ; 
  end function GetAlertLogAlertPrefix ;
  
  ------------------------------------------------------------
  impure function GetAlertLogLogPrefix            return string is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogLogPrefix ; 
  end function GetAlertLogLogPrefix ;

  ------------------------------------------------------------
  impure function GetAlertLogReportPrefix return string is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogReportPrefix ; 
  end function GetAlertLogReportPrefix ;
  
  ------------------------------------------------------------
  impure function GetAlertLogDoneName return string is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogDoneName ; 
  end function GetAlertLogDoneName ;

  ------------------------------------------------------------
  impure function GetAlertLogPassName return string is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogPassName ; 
  end function GetAlertLogPassName ;

  ------------------------------------------------------------
  impure function GetAlertLogFailName return string is
  ------------------------------------------------------------
  begin
    return AlertLogStruct.GetAlertLogFailName ; 
  end function GetAlertLogFailName ;
  
  ------------------------------------------------------------
  function IsLogEnableType (Name : String) return boolean is
  ------------------------------------------------------------
  -- type LogType is (ALWAYS, DEBUG, FINAL, INFO, PASSED) ;  -- NEVER
  begin
    if    Name = "PASSED" then return TRUE ; 
    elsif Name = "DEBUG" then return TRUE ; 
    elsif Name = "FINAL" then return TRUE ;
    elsif Name = "INFO"  then return TRUE ; 
    end if ; 
    return FALSE ;
  end function IsLogEnableType ; 

  ------------------------------------------------------------
  procedure ReadLogEnables (file AlertLogInitFile : text) is
  --  Preferred Read format
  --  Line 1:  instance1_name log_enable log_enable log_enable
  --  Line 2:  instance2_name log_enable log_enable log_enable
  --  when reading multiple log_enables on a line, they must be separated by a space
  -- 
  --- Also supports alternate format from Lyle/....
  --  Line 1:  instance1_name
  --  Line 2:  log enable
  --  Line 3:  instance2_name
  --  Line 4:  log enable
  --
  ------------------------------------------------------------
    type     ReadStateType is (GET_ID, GET_ENABLE) ; 
    variable ReadState        : ReadStateType := GET_ID ; 
    variable buf              : line ;
    variable Empty            : boolean ;
    variable MultiLineComment : boolean := FALSE ;
    variable Name             : string(1 to 80) ;
    variable NameLen          : integer ; 
    variable AlertLogID       : AlertLogIDType ; 
    variable ReadAnEnable     : boolean ;
    variable LogLevel         : LogType ;
  begin
    ReadState := GET_ID ; 
    ReadLineLoop : while not EndFile(AlertLogInitFile) loop
      ReadLine(AlertLogInitFile, buf) ; 
      if ReadAnEnable then 
        -- Read one or more enable values, next line read AlertLog name
        -- Note that any newline with ReadAnEnable TRUE will result in 
        -- searching for another AlertLogID name - this includes multi-line comments.
        ReadState := GET_ID ; 
      end if ; 
      
      ReadNameLoop : loop 
        EmptyOrCommentLine(buf, Empty, MultiLineComment) ;
        next ReadLineLoop when Empty ; 
        
        case ReadState is
          when GET_ID => 
            sread(buf, Name, NameLen) ; 
            exit ReadNameLoop when NameLen = 0 ; 
            AlertLogID := GetAlertLogID(Name(1 to NameLen), ALERTLOG_ID_NOT_ASSIGNED) ;   
            ReadState := GET_ENABLE ; 
            ReadAnEnable := FALSE ;   
            
          when GET_ENABLE => 
            sread(buf, Name, NameLen) ; 
            exit ReadNameLoop when NameLen = 0 ; 
            ReadAnEnable := TRUE ;   
            if not IsLogEnableType(Name(1 to NameLen)) then 
              Alert(OSVVM_ALERTLOG_ID, "AlertLogPkg.ReadLogEnables: Found Invalid LogEnable: " & Name(1 to NameLen)) ; 
              exit ReadNameLoop ;
            end if ; 
            LogLevel := LogType'value(Name(1 to NameLen)) ; 
            SetLogEnable(AlertLogID, LogLevel, TRUE) ;
        end case ; 
      end loop ReadNameLoop ; 
    end loop ReadLineLoop ; 
  end procedure ReadLogEnables ;
  
  ------------------------------------------------------------
  procedure ReadLogEnables (FileName : string) is
  ------------------------------------------------------------
    file AlertLogInitFile : text open READ_MODE is FileName ;
  begin
    ReadLogEnables(AlertLogInitFile) ;
  end procedure ReadLogEnables ;
  
  ------------------------------------------------------------
  function PathTail (A : string) return string is 
  ------------------------------------------------------------
    alias aA : string(1 to A'length) is A ;
  begin
    for i in aA'length - 1 downto 1 loop 
      if aA(i) = ':' then 
        return aA(i+1 to aA'length-1)  ; 
      end if ; 
    end loop ;
    return aA ; 
  end function PathTail ; 
 
end package body AlertLogPkg ;