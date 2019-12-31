library ieee;
use ieee.std_logic_1164.all;

entity dff02 is
  port (q : out std_logic;
        d : std_logic;
        en : std_logic;
        rst : std_logic;
        clk : std_logic);
end dff02;

architecture behav of dff02 is
  signal t : std_logic := '1';
begin
  process (clk, rst) is
  begin
    if rst = '1' then
      t <= '1';
    elsif rising_edge (clk) then
      if en = '1' then
        t <= d;
      end if;
    end if;
  end process;

  q <= t;
end behav;
