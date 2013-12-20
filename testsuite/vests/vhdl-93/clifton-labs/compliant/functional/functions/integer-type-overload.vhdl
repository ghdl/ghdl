entity test is
end test;

architecture only of test is
  type integer_t1 is range 0 to 2;
  type integer_t2 is range 2 to 4;
  function test_function ( constant param : integer_t1 )
    return boolean is
  begin
    return true;
  end function;
  
  function test_function ( constant param : integer_t2 )
    return boolean is
  begin
    return true;
  end function;
begin  -- only
  test: process
    variable result : boolean;
    variable param1 : integer_t1 := 3;
    variable param2 : integer_t2 := 5;
  begin  -- process
    result := test_function( param1 );
    result := test_function( param2 );
    wait;
  end process;
end only;
