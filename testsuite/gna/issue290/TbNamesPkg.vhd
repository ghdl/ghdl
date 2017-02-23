--
--  File Increment:         TbNamesPkg.vhd
--  Design Unit Increment:  TbNamesPkg
--  Revision:          STANDARD VERSION
--
--  Maintainer:        Jim Lewis      email:  jim@synthworks.com
--  Contributor(s):
--     Jim Lewis          SynthWorks
--
--
--  Package Defines
--      Data structure for Increment. 
--
--  Developed for:
--        SynthWorks Design Inc.
--        VHDL Training Classes
--        11898 SW 128th Ave.  Tigard, Or  97223
--        http://www.SynthWorks.com
--
--
--  Revision History:
--    Date      Version    Description
--    05/2015   2015.06    Added input to Get to return when not initialized
--
--
--  Copyright (c) 2010 - 2016 by SynthWorks Design Inc.  All rights reserved.
--

package TbNamesPkg is
  type IncrementPType is protected
    procedure Inc ;
    impure function Get return integer ;
  end protected IncrementPType ;
  
  procedure PrintNames ;
  procedure CallPrintNames ; 
end package TbNamesPkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body TbNamesPkg is
  type IncrementPType is protected body
    variable IncrementVar : integer := 0 ;
    
    impure function PrintNamesFun(S : string) return integer is
    begin 
      report "IncrementVar'INSTANCE_NAME as a parameter: " & S ;
      report "IncrementVar: INSTANCE_NAME  " & IncrementVar'INSTANCE_NAME  ;
      report "IncrementVar: PATH_NAME      " & IncrementVar'PATH_NAME  ;
      report "function PrintNamesFun: INSTANCE_NAME  " & PrintNamesFun'INSTANCE_NAME  ;
      report "function PrintNamesFun: PATH_NAME      " & PrintNamesFun'PATH_NAME  ;
      return 0 ; 
    end function PrintNamesFun ; 
   
    variable Temp : integer := PrintNamesFun(IncrementVar'INSTANCE_NAME) ;

    ------------------------------------------------------------
    procedure Inc is
    ------------------------------------------------------------
    begin
      IncrementVar := IncrementVar + 1 ;
    end procedure Inc ;

    ------------------------------------------------------------
    impure function Get return integer is
    ------------------------------------------------------------
    begin
      report "IncrementVar: INSTANCE_NAME  " & IncrementVar'INSTANCE_NAME  ;
      report "IncrementVar: PATH_NAME      " & IncrementVar'PATH_NAME  ;
      report "Method Get: INSTANCE_NAME  " & Get'INSTANCE_NAME  ;
      report "Method Get: PATH_NAME      " & Get'PATH_NAME  ;
      return IncrementVar ; 
    end function Get ;
  end protected body IncrementPType ;

  
  procedure PrintNames is
  begin 
    report "procedure PrintNames: INSTANCE_NAME  " & PrintNames'INSTANCE_NAME  ;
    report "procedure PrintNames: PATH_NAME      " & PrintNames'PATH_NAME  ;
  end procedure PrintNames ; 
  
  procedure CallPrintNames is 
  begin
    PrintNames ;
  end procedure CallPrintNames ;
end package body TbNamesPkg ;
