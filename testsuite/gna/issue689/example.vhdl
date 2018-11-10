library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package test_pkg is
  type my_array is array(natural range <>) of unsigned;
end package test_pkg;

--library vunit_lib;
--context vunit_lib.vunit_context;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.test_pkg.all;

entity tb_example is
  generic (runner_cfg : string := "x");
end entity;

architecture tb of tb_example is
  constant DDR_DATA_WIDTH       : integer := 128;
  constant FILTER_WORD_SIZE     : integer := 16;

--  signal switch_out : my_array(0 to DDR_DATA_WIDTH / FILTER_WORD_SIZE - 1)(natural(log2(real(DDR_DATA_WIDTH / FILTER_WORD_SIZE))) - 1 downto 0);
  signal switch_out : my_array(0 to 7)(2 downto 0);
    
begin
  main : process
  begin
 --   test_runner_setup(runner, runner_cfg);
    report "Hello world!";
 --   test_runner_cleanup(runner); -- Simulation ends here
    wait;
  end process;
end architecture;
