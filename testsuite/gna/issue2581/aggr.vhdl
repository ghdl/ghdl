library IEEE ;
use ieee.std_logic_1164.all ; 
use ieee.numeric_std_unsigned.all ;

-- library osvvm ; 
-- context osvvm.OsvvmContext ;

entity aggregate_issue is 
end entity aggregate_issue ; 
architecture test of aggregate_issue is 

--  signal TestDone : BarrierType ; 
  signal SLV1, SLV2 : std_logic_vector(7 downto 0) ;
  signal Index : integer := 0 ; 
begin
--   ControlProc : process
--   begin
--     SetTestName("aggregate_issue") ; 
--     SetLogEnable(PASSED, TRUE) ; 
--     WaitForBarrier(TestDone, 1 ms) ;
--     EndofTestReports ;  
--     std.env.stop ; 
--     wait ;
--   end process ControlProc ; 
 
 
    
  SLV1 <= (SLV1'left downto Index+1 => '0') & '1' & (Index-1 downto 0 => '0'); 

  RtlProc : process(Index)
  begin
    SLV2 <= (SLV2'left downto Index+1 => '0') & '1' & (Index-1 downto 0 => '0'); 
  end process RtlProc;

  
  TestProc : process
    variable expected : std_logic_vector(7 downto 0) ;

  begin
    wait for 0 ns ;  -- Allow SetLogEnable to be set 
    wait for 0 ns ;
    
    for i in SLV1'range loop 
      Index <= i ;
      wait for 10 ns ; 
      Expected := to_slv(2**Index, SLV1'length) ; 
      -- AffirmIfEqual(SLV1, Expected, "SLV1:") ;
      if SLV1 = expected then 
        report "SLV1: " & to_hstring(SLV1) & ",  expected: " & to_hstring(expected) severity note ; 
      else 
        report "SLV1: " & to_hstring(SLV1) & ",  expected: " & to_hstring(expected) severity error ; 
      end if ; 
      -- AffirmIfEqual(SLV2, Expected, "SLV2:") ;
      if SLV2 = expected then 
        report "SLV2: " & to_hstring(SLV2) & ",  expected: " & to_hstring(expected) severity note ; 
      else 
        report "SLV2: " & to_hstring(SLV2) & ",  expected: " & to_hstring(expected) severity error ; 
      end if ; 
    end loop ; 
    
    -- WaitForBarrier(TestDone) ;
    wait ; 

  end process TestProc ; 
end architecture test ; 
