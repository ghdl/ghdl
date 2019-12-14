library ieee;
use ieee.std_logic_1164.all;

entity dff06 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic;
        rst : std_logic);
end dff06;

architecture behav of dff06 is
  signal p : std_logic_vector(7 downto 0);
begin
  process (clk, rst) is
  begin
    if rst = '1' then
      p <= x"00";
    elsif rising_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;
