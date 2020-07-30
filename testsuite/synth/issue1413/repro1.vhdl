library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_Std.all;

entity repro1 is
  port (res : out unsigned(7 downto 0));
end;

architecture behav of repro1 is
  constant left : unsigned := x"71";
  constant right : unsigned := x"03";
  constant r : unsigned := left / right;
begin
  res <= r;
end behav;
