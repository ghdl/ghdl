library ieee;
use ieee.std_logic_1164.all;

entity dff01 is
  port (q : out std_logic_vector (3 downto 0);
        d : std_logic_vector (3 downto 0);
        en : std_logic;
        rst : std_logic;
        clk : std_logic);
end dff01;

architecture behav of dff01 is
  signal t : std_logic_vector (7 downto 0);
  signal a : std_logic_vector (3 downto 0);
begin
  a <= d xor b"0101";
  
  process (clk) is
  begin
    if rst = '1' then
      q <= x"0";
    elsif rising_edge (clk) then
      if en = '1' then
        q <= d;
        t (7 downto 4) <= a;
      end if;
    end if;
  end process;
end behav;
