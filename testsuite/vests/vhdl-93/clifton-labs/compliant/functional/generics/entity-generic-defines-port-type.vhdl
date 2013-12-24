entity test_bench is
end test_bench;

entity generic_defines_port_type is
  generic( width : natural );
  port( input : in bit_vector( width - 1 downto 0 );
        finished : in boolean );
end entity;

architecture only of generic_defines_port_type is

begin  -- only
  p: process( finished )
  begin  -- process p
    if finished = true then
      for i in input'range loop
        assert input(i) = '1' report "TEST FAILED" severity FAILURE;
      end loop;  -- i
    end if;
  end process p;
end only;

architecture only of test_bench is
  signal gdpt1_input : bit_vector( 3 downto 0 ) := "0000";
  signal gdpt1_finished : boolean := false;
begin  -- only
  gdpt1: entity work.generic_defines_port_type
    generic map ( width => 4 )
    port    map ( input => gdpt1_input, finished => gdpt1_finished );

  doit: process
  begin  -- process doit
    gdpt1_input <= "1111";
    wait for 1 fs;
    gdpt1_finished <= true;
    wait for 1 fs;
    report "TEST PASSED";
    wait;
  end process doit;
  
end only;
