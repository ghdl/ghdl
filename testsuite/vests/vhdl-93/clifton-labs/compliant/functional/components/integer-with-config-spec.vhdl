entity forty_two is
  port (
    int_out : out integer);
end forty_two;

architecture only of forty_two is
begin  -- only
  process
  begin  -- process
    int_out <= 42;
    wait;
  end process;
end only;

entity test_bench is
end test_bench;

architecture only of test_bench is

  component forty_two
    port (
      int_out : out integer);
  end component;

  for ft0 : forty_two
    use entity work.forty_two(only)
      port map ( int_out => int_out );
  
  signal int_signal : integer;

begin  -- only

  ft0 : component forty_two
    port map (
      int_out => int_signal );


  test: process
  begin  -- process test
    wait for 1 ms;
    assert int_signal = 42 report "TEST FAILED" severity ERROR;
    assert not(int_signal = 42) report "TEST PASSED" severity NOTE;
    wait;
  end process test;
  
end only;
