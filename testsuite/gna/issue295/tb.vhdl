library vunit_lib;
context vunit_lib.vunit_context;

entity tb_test4 is
  generic (
    runner_cfg : string);
end tb_test4;

architecture tb of tb_test4 is
begin
    test_runner : process
    begin
      test_runner_setup(runner, runner_cfg);

      assert 2.0**(-1023) > 0.0 report "This passes";
      assert 2.0**(-1024) > 0.0 report "This fails";

      test_runner_cleanup(runner);
      wait;
    end process test_runner;
end;
