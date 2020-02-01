library ieee;
use ieee.std_logic_1164.all;

entity dff08b is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic;
        en : std_logic;
        rst : std_logic);
end dff08b;

architecture behav of dff08b is
  signal p : std_logic_vector(7 downto 0) := x"aa";
begin
  process (clk, rst) is
  begin
    if en = '0' then
      null;
    elsif rst = '1' then
      p <= x"00";
    elsif rising_edge (clk) then
      p <= d;
    end if;
  end process;
  q <= p;
end behav;
