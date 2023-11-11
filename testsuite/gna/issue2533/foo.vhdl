library ieee;
use ieee.std_logic_1164.all;

package foo is
  component foobar is
    port (
      g1a, g1b : in  std_logic;
      g1q_n    : out std_logic
      );
  end component;
end;

package body foo is
end;
