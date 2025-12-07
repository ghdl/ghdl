library ieee;
use ieee.std_logic_1164.all;

entity dff24 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic_vector(0 to 1));
end;

architecture behav of dff24 is
--  alias clk1 : std_logic is clk;
begin
  process (clk) is
  begin
    if clk(0)'event and clk(0) = '0' then
      q <= d;
    end if;
  end process;
end behav;
