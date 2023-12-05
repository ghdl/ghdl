library ieee;
use ieee.std_logic_1164.all;

entity reg_ev1 is
  port (
    clk : in std_logic;
    d : in std_logic;
    q : out std_logic);
end;

architecture behav of reg_ev1 is
begin
  process (clk)
  begin
    if clk = '1' and clk'event then
      q <= d;
    end if;
  end process;
end behav;
