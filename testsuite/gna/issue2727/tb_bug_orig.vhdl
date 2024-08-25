library vunit_lib;
context vunit_lib.vunit_context;

library osvvm;
use osvvm.RandomPkg.RandomPType;

entity tb_bug is
  generic (runner_cfg : string);
end entity;

architecture tb of tb_bug is
begin
  main : process
    variable rnd : RandomPType;
    variable value : real;
  begin
    test_runner_setup(runner, runner_cfg);

    value := rnd.Uniform(0.0, 1.0);
    assert value >= 0.0 report "Got " & to_string(value) severity error;
    
    test_runner_cleanup(runner);
  end process;
end architecture;
