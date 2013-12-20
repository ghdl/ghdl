entity test is
end test;

architecture only of test is

begin  -- only
  doit: process
    variable one, two, three : boolean := false;
  begin  -- process doit
    if true then
      one := true;
    else
      
    end if;
    
    if false then
      one := false;
    else
      two := true;
    end if;

    if false then
      one := false;
    elsif true then
      three := true;
    else
      two := false;
    end if;
    
    assert one report "TEST FAILED - first if test failed" severity failure;
    assert two report "TEST FAILED - second if test failed" severity failure;
    assert three report "TEST FAILED - third if test failed" severity failure;
    report "TEST PASSED" severity note;
    
    wait;
  end process doit;
end only;
