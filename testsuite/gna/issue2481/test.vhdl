library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package test_pkg is
  signal sig_pkg : std_logic := '0';
  procedure force_pulse_pkg;
end package test_pkg;

package body test_pkg is
    procedure force_pulse_pkg is
    begin
      sig_pkg <= force '1';
      wait for 1 ps;
      assert sig_pkg = '1' report "force_pulse_pkg" severity failure;
      sig_pkg <= release;
      wait for 1 ps;
    end procedure;
end package body;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.test_pkg.all;

entity test is
end entity test;

architecture beh of test is
   signal sig : std_logic := '0';
begin
  
  process
    alias sig_a is sig_pkg;
    procedure force_pulse(
      signal target         : inout std_logic) is
    begin
      target <= force '1';
      wait for 1 ps;
      assert target = '1' report "force_pulse" severity failure;
      target <= release;
      wait for 1 ps;
    end procedure;
  begin
    assert sig = '0' report "Init value" severity failure;
    sig <= force '1';
    wait for 1 ps;
    assert sig = '1' report "Force 1" severity failure;
    sig <= release;
    wait for 1 ps;
    assert sig = '0' report "Release value " & to_string(sig) severity failure;
    
    assert sig_pkg = '0' report "Init value pkg" severity failure;
    sig_pkg <= force '1';
    wait for 1 ps;
    assert sig_pkg = '1' report "Force 1 pkg" severity failure;
    sig_pkg <= release;
    wait for 1 ps;
    assert sig_pkg = '0' report "Release pkg value " & to_string(sig_pkg) severity failure;
    
    assert sig_a = '0' report "Init value alias" severity failure;
    sig_a <= force '1';
    wait for 1 ps;
    assert sig_a = '1' report "Force 1 alias" severity failure;
    sig_a <= release;
    wait for 1 ps;
    assert sig_a = '0' report "Release alias value " & to_string(sig_a) severity failure;

    force_pulse_pkg;
    assert sig_a = '0' report "Release procedure package value " & to_string(sig_a) severity failure;

    force_pulse(sig_a);
    assert sig_a = '0' report "Release procedure value " & to_string(sig_a) severity failure;
    wait;
  end process;

end architecture beh;
