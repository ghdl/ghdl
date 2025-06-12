library IEEE ;
  use ieee.std_logic_1164.all ;
  use ieee.numeric_std.all ;

  use std.textio.all ;
  use ieee.std_logic_textio.all ;

library osvvm ;
  context osvvm.OsvvmContext ;

entity GetReqID_1 is
end GetReqID_1 ;
architecture TestCase of GetReqID_1 is

  -- From AlertLogPkg
  --   subtype  AlertLogIDType           is integer ;

  signal ID1, ID2 : AlertLogIDType ;     
begin

  TestProc : process 
  begin
  
    SetTestName("GetReqID_1") ; 
    SetLogEnable(PASSED, TRUE) ; 

    ID1 <= GetReqID("ID", 1, REQUIREMENT_ALERTLOG_ID) ; 
-- works    ID1 <= NewReqID("ID", 1) ; 
    ID2 <= GetReqID("ID", 1) ; 
-- works   ID1 <= NewReqID("ID", 1, REQUIREMENT_ALERTLOG_ID) ; 

    wait for 0 ns ; 
    print("AlertLogID: " & to_string(ID1)) ;

    AffirmIf(ID1, TRUE, "Token Pass") ; 
   
    EndOfTestReports ; 

    std.env.stop ; 
    wait ;  
    
  end process TestProc ;
end TestCase ;
