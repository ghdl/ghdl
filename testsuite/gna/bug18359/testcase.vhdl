entity testcase is
end entity testcase;

architecture non_conformity of testcase is
  function some_function return integer is
    variable my : integer := 0;
  begin
    my := my + 1;
    return my;
  end function some_function;
begin

  stuff: process
    variable last_value : integer;
  begin
    for i in 1 to 4 loop
      report "Count: " & integer'image(some_function)
        severity NOTE;
    end loop;
    last_value := some_function;
    assert last_value = 5
      report "Counter's last value is " & integer'image(last_value)
      &      " but expected 5"
      severity ERROR;
    wait;
  end process;

end architecture non_conformity;
