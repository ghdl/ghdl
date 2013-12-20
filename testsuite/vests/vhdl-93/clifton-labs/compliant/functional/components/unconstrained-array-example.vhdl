entity forty_two is
  port (
    bv_out : out bit_vector );
end forty_two;

architecture only of forty_two is
begin  -- only
  process
  begin  -- process
    bv_out <= "0110";
    wait;
  end process;
end only;

entity test_bench is
end test_bench;

architecture only of test_bench is

  component forty_two_component
    port (
      c_bv_out : out bit_vector );
  end component;

  for ft0 : forty_two_component
    use entity work.forty_two(only)
      port map (
        bv_out => c_bv_out );
  
  signal bv_signal : bit_vector( 3 downto 0 );

begin  -- only

  ft0 : component forty_two_component
    port map (
      c_bv_out => bv_signal );


  test: process
  begin  -- process test
    wait for 1 ms;
    assert bv_signal = "0110" report "TEST FAILED" severity ERROR;
    assert not(bv_signal = "0110") report "TEST PASSED" severity NOTE;
    wait;
  end process test;
  
end only;
