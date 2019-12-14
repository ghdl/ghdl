library ieee;
use ieee.std_logic_1164.all;

entity dff02 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic;
        rstn : std_logic);
end dff02;

architecture behav of dff02 is
begin
  process (clk, rstn) is
  begin
    if rstn = '0' then
      q <= '0';
    elsif rising_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;
