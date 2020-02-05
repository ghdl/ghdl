library ieee;

use ieee.std_logic_1164.all;

entity unconnected is
  port (
    output: out std_logic
    );
end entity;

architecture arch of unconnected is
  signal no_value: std_logic;
begin
  output <= no_value;
end;
