library ieee;
--library vunit_lib;
--context vunit_lib.vunit_context; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity tb_counter is
  generic (runner_cfg : string);
end tb_counter;

architecture arch_tb_counter of tb_counter is
  component counter is
    port (
      key0: in std_logic;
      key3: in std_logic;
      counter_out: out std_logic_vector(3 downto 0)
    );
  end component;
  signal key0, key3: std_logic;
  signal counter_out: std_logic_vector(3 downto 0);

  function trigger_rising() return std_logic_vector is
    begin
      key0 <= '0';
      wait for 1 ns;
      key0 <= '1';
      wait for 1 ns;
    end;
    
begin
  uut: counter port map(
    key0 => key0,
    key3 => key3,
    counter_out => counter_out
  );

  main: process
  begin
    test_runner_setup(runner, runner_cfg);
    for j in 0 to 8 loop
      trigger_rising();
      check_match(counter_out, (std_logic_vector(to_unsigned(j + 1, 4))));
    end loop;
    check_match(counter_out, ())))
    test_runner_cleanup(runner); -- Simulation ends here
  end process;

end arch_tb_counter  ; -- arch_tb_counter 
