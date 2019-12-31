library ieee;
use ieee.std_logic_1164.all;

entity dff04 is
  port (q : out std_logic_vector (3 downto 0);
        d : std_logic_vector (3 downto 0);
        en : std_logic;
        rst : std_logic;
        clk : std_logic);
end dff04;

architecture behav of dff04 is
begin
  process (clk) is
  begin
    if rst = '0' then
      null;
    elsif rising_edge (clk) then
      if en = '1' then
        q <= d;
      end if;
    end if;
  end process;
end behav;
