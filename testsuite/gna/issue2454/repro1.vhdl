library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity bug_test is
end entity;

architecture tb of bug_test is
begin
	main : process
          variable test, vu : ufixed(1 downto -5);
          variable test_s, vs : sfixed(1 downto -5);
	begin
          --  0.0000_0100_0111_0111_1100
          test := to_ufixed(0.017452406, test);
          vu := to_ufixed(0.03125, test);
          assert test = vu report "Check ufixed failed: 0.017452406";

          --  0.0000_0100_1111_1111_1111
          test := to_ufixed(0.019531249999, test);
          assert test = vu report "Check ufixed failed: 0.019531249999";

          --  0.0000_0101_0000_0000_0000
          test := to_ufixed(0.01953125, test);
          assert test = vu report "Check ufixed failed: 0.01953125";

          test_s := to_sfixed(0.017452406, test_s);
          vs := to_sfixed(0.03125, test_s);
          assert test_s = vs report "Check sfixed failed: 0.017452406";

          test_s := to_sfixed(0.019531249999, test_s);
          assert test_s = vs report "Check sfixed failed: 0.019531249999";

          test_s := to_sfixed(0.01953125, test_s);
          assert test_s = vs report "Check failed: 0.01953125";
          wait;
	end process;
end architecture;
