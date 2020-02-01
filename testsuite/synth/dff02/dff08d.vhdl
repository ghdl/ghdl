library ieee;
use ieee.std_logic_1164.all;

entity dff08d is
  port (q : out std_logic_vector(7 downto 0);
        d : std_logic_vector(7 downto 0);
        clk : std_logic;
        en : std_logic;
        rst : std_logic);
end dff08d;

architecture behav of dff08d is
  constant c : std_logic_vector(7 downto 0) := x"aa";
  signal p : std_logic_vector(7 downto 0) := c;
begin
  process (clk, rst) is
  begin
    if rst = '1' then
      p <= c;
    elsif rising_edge (clk) then
      p <= d;
    end if;
  end process;
  q <= p;
end behav;
