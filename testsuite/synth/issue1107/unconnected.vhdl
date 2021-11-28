library ieee;

use ieee.std_logic_1164.all;

entity unconnected is
  port (
    outp: out std_logic
    );
end entity;

architecture arch of unconnected is
  signal no_value: std_logic;
begin
  outp <= no_value;
end;
