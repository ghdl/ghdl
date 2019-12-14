library ieee;
use ieee.std_logic_1164.all;

entity dff12 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic;
        rstn : std_logic);
end dff12;

architecture behav of dff12 is
  signal ff : std_logic := '1';
begin
  process (clk, rstn) is
  begin
    if rising_edge (clk) then
      if rstn = '0' then
        ff <= '0';
      else
        ff <= d;
      end if;
    end if;
  end process;
  q <= ff;
end behav;
