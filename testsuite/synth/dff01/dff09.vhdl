library ieee;
use ieee.std_logic_1164.all;

entity dff09 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic;
        rstn : std_logic);
end dff09;

architecture behav of dff09 is
begin
  process (clk, rstn) is
    constant rval : std_logic := '0';
  begin
    if rstn = '0' then
      q <= rval;
    elsif rising_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;
