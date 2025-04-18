library ieee;
use ieee.std_logic_1164.all;

package repro2_pkg is
  type my_rec is record
    rdy, rst : std_logic;
  end record;
end repro2_pkg;
