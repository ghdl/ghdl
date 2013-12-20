entity test is
end test;


architecture only of test is

  type month_name IS (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec );
  type date IS
    record
      day   : integer range 1 to 31;
      month : month_name;
      year  : integer range 0 to 4000;
    end record;

begin  -- only
  p: process
    constant christmas : date := ( 25, Dec, 0 );
  begin  -- process p
    assert christmas.day = 25 report "TEST FAILED" severity FAILURE;
    assert christmas.month = Dec report "TEST FAILED" severity FAILURE;
    assert christmas.year = 0 report "TEST FAILED" severity FAILURE;
    report "TEST PASSED" severity NOTE;
    wait;
  end process p;
end only;
