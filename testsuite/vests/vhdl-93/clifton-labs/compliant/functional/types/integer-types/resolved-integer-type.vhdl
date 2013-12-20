entity test is
end test;

architecture only of test is
  type integer_array is array ( natural range <> ) of integer;
  
  function return_biggest ( inputs : integer_array )
    return integer is
    variable retval : integer := integer'left;
  begin
    for i in inputs'range loop
      if inputs(i) > retval then
        retval := inputs(i);
      end if;
    end loop;  -- i
    return retval;
  end return_biggest;
    
  subtype biggest_wins is return_biggest integer;

  signal common : biggest_wins;
  
begin  -- only

  p1 : process
  begin
    common <= 1 after 1 ns;
    wait;
  end process;    

  p2 : process
  begin
    common <= 1 after 1 ns;
    wait;
  end process;

  test: process
  begin
    wait for 2 ns;
    assert common = 1 report "TEST FAILED" severity failure;
    wait;
  end process;    
  
end only;
