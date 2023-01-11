library ieee;
use ieee.std_logic_1164.all;

entity repro is
end;

architecture behav of repro is
  constant c : bit := '0';
  signal s : std_logic := to_X01(c);
begin
end behav;
