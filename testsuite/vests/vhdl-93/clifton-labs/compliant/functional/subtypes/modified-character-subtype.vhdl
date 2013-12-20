entity test is
end test;

architecture only of test is
begin  -- only
  doit: process
    subtype sub_character is character range 'A' to 'Z';
    constant x : sub_character;
  begin
    assert x = sub_character'left report "TEST FAILED" severity FAILURE;
    report "TEST PASSED";
    wait;
  end process;   
  
end only;
