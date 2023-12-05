library ieee;
use ieee.std_logic_1164.all;

entity reg_err1 is
  port (
    clk : in std_logic;
    rst : std_logic;
    d : in std_logic;
    q : out std_logic);
end;

architecture behav of reg_err1 is
begin
  process (clk)
  begin
    if rst = '1' then
      q <= '0';
    elsif rising_edge(clk) then
      q <= d;
    end if;
  end process;
end behav;
