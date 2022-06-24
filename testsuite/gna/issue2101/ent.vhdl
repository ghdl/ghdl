library ieee;
use ieee.std_logic_1164.all;

entity ent is
  port (
    clk: in std_logic;
    reset: in std_logic);
end entity;

architecture a of ent is
begin
  foo: process(clk, reset)
  variable counter: integer range 0 to 15;
  begin
    if reset = '1' then
        counter := counter'high;
    elsif rising_edge(clk) then
        counter := counter - 1;
    end if;
  end process;
end;
