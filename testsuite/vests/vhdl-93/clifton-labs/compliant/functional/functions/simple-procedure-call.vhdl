entity test is
end test;

architecture only of test is
  procedure doit is
  begin
    report "PROCEDURE CALLED!";
  end procedure;
begin  -- only
  process
  begin  -- process doit
    doit;
    report "TEST PASSED";
    wait;    
  end process;
end only;
