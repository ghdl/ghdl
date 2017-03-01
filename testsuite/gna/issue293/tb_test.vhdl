library vunit_lib;
context vunit_lib.vunit_context;

library ieee;
use ieee.float_pkg.all;

entity tb_test is
  generic (
    runner_cfg : string);
end tb_test;

architecture tb of tb_test is
begin
    test_runner : process
      alias fp32 is float32;
    begin
      test_runner_setup(runner, runner_cfg);

      assert zerofp = neg_zerofp;

      test_runner_cleanup(runner);
      wait;
    end process test_runner;
end;
