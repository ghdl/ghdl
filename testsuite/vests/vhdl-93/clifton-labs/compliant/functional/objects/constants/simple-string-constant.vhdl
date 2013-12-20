entity test is
end test;

architecture only of test is
begin  -- only
  doit: process
    constant string_constant : string := "init";
  begin  -- process
    assert string_constant(1) = 'i' REPORT "string_constant(1) not properly intialized" SEVERITY FAILURE;
    assert string_constant(2) = 'n' REPORT "string_constant(2) not properly intialized" SEVERITY FAILURE;
    assert string_constant(3) = 'i' REPORT "string_constant(3) not properly intialized" SEVERITY FAILURE;
    assert string_constant(4) = 't' REPORT "string_constant(4) not properly intialized" SEVERITY FAILURE;
    wait;
  end process;
end only;
