library ieee;
use ieee.std_logic_1164.all;

entity dff08 is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic;
        en : std_logic;
        rst : std_logic);
end dff08;

architecture behav of dff08 is
  signal p : std_logic_vector(7 downto 0);
begin
  process (clk, rst) is
  begin
    if en = '0' then
      null;
    elsif rst = '1' then
      q <= x"00";
    elsif rising_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;
