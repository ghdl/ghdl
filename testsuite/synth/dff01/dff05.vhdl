library ieee;
use ieee.std_logic_1164.all;

entity dff05 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic;
        rst : std_logic;
        en : std_logic);
end dff05;

architecture behav of dff05 is
begin
  process (clk, rst) is
  begin
    if rst = '1' then
      q <= x"00";
    elsif rising_edge (clk) then
      if en = '1' then
        q <= d;
      end if;
    end if;
  end process;
end behav;
