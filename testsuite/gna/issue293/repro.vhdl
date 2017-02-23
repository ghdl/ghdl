library ieee;
use ieee.float_pkg.all;

entity tb_test is
  generic (
    runner_cfg : string := "h");
end tb_test;

architecture tb of tb_test is
begin
  test_runner : process
      --  Makes the implicit "=" directly visible.
      alias fp32 is float32;
    begin
      assert not (zerofp = neg_zerofp) severity failure;

      wait;
    end process test_runner;

    test_runner2 : process
        --  Only the user-defined "=" is visible.
    begin
      assert zerofp = neg_zerofp severity failure;

      wait;
    end process test_runner2;
end;
