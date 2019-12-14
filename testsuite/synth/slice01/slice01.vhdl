library ieee;
use ieee.std_logic_1164.all;

entity slice01 is
  generic (w: natural := 4);
  port (rst : std_logic;
        clk : std_logic;
        di : std_logic;
        do : out std_logic_vector (w - 1 downto 0));
end slice01;

architecture behav of slice01 is
  signal r : std_logic_vector (w - 1 downto 0);
begin
  do <= r;
  
  process(clk)
  begin
    if rising_edge (clk) then
      if rst = '1' then
        r <= (others => '0');
      else
        r (w - 2 downto 0) <= r (w - 1 downto 1);
        r (w - 1) <= di;
      end if;
    end if;
  end process;
end behav;
