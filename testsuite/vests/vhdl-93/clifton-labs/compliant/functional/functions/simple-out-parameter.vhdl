entity test is
end test;

architecture only of test is
  procedure out_param ( one : out integer ) is
  begin
    one := 1;
  end out_param;
begin  -- only
  doit: process
    variable one : integer := 0;
  begin  -- process doit
    out_param( one );
    assert one = 1 report "TEST FAILED" severity failure;
    report "TEST PASSED";
    wait;
  end process doit;
end only;
