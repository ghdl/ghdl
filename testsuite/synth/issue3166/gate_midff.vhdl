library ieee;
use ieee.std_logic_1164.all;

entity gate_midff is
  port (clk : std_logic;
        d : std_logic;
        els : std_logic;
        init : std_logic;
        q : out std_logic);
end gate_midff;
