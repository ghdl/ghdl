library ieee;
use ieee.std_logic_1164.all;

package utils is
  type slv_array is array(natural range <>) of std_logic_vector;

  function slv_or (vv: slv_array) return std_logic_vector;
end utils;

package body utils is
--  function slv_or is (vv: slv_array) return std_logic_vector
  function slv_or (vv: slv_array) return std_logic_vector
  is
    variable res : std_logic_vector(vv (vv'left)'range);
  begin
    res := (others => '0');
    for i in vv'range loop
      res := res or vv (i);
    end loop;
    return res;
  end slv_or;
end utils;


entity repro1 is
end;

library ieee;
use ieee.std_logic_1164.all;

use work.utils.all;

architecture behav of repro1 is
begin
  process
    constant c : slv_array := (x"01", x"e0");
    variable v : std_logic_vector(7 downto 0);
  begin
    v := slv_or (c);
    assert v = x"e1" severity failure;
    wait;
  end process;
end;


