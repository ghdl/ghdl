library ieee;
use ieee.std_logic_1164.std_logic;
use ieee.std_logic_1164.to_x01;
use ieee.std_logic_1164.is_x;

package std_logic_warning is
  function "="(l, r : std_logic) return boolean;
end package;

package body std_logic_warning is
    
    use ieee.std_logic_1164."=";
    
    function "="(l, r : std_logic) return boolean is
    begin
    if is_x(l) or is_x(r) then
      report "std_logic_warning.""="": metavalue detected, returning FALSE"
        severity WARNING;
      return FALSE;
    end if;
    return l = r; -- std_logic_1164."="(l, r);
    end function;

end package body;

library ieee;
use ieee.std_logic_1164.std_ulogic;
use ieee.std_logic_1164.std_logic;
-- use ieee.std_logic_1164.all;

use work.std_logic_warning.all;
entity warning_test is
end entity;

architecture foo of warning_test is
    signal a: std_logic;
    signal b: std_logic;
begin

UNLABELLED:
    process
    begin
        wait for 1 ns;
        a <= ieee.std_logic_1164.'X';
        wait for 1 ns;
        b <= ieee.std_logic_1164.'1';
        wait for 1 ns;
        a <= ieee.std_logic_1164.'0';
        wait for 1 ns;
        b <= ieee.std_logic_1164.'0';
        wait;
    end process;
    
MONITOR:
    process (a,b)
    begin
        assert a = b 
            report "a = b " & "( " & std_logic'image(a)
                             & "=" & std_logic'image(b) & " )"
                severity NOTE;
    end process;

end architecture;
    
