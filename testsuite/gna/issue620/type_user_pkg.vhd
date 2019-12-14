library ieee;
use ieee.std_logic_1164.all;

package type_user_pkg is
  generic (
    type thetype;
    function transition(val : thetype) return thetype

    );

  procedure unity_proc(signal clk : in std_ulogic; signal inVal : in thetype; signal outVal : out theType);
end package type_user_pkg;

package body type_user_pkg is

  procedure unity_proc(signal clk : in std_ulogic; signal inVal : in thetype; signal outVal : out theType) is
  begin
    wait until rising_edge(clk);
    outVal <= transition(inVal);
  end procedure unity_proc;
  
end package body;
