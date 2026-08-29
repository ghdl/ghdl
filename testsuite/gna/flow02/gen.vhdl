library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity counter is
  generic (
    WIDTH : positive := 8;
    STEP  : natural  := 1
  );
  port (
    clk : in  std_logic;
    q   : out std_logic_vector (WIDTH - 1 downto 0)
  );
end counter;

architecture rtl of counter is
  signal cnt : unsigned (WIDTH - 1 downto 0) := (others => '0');
begin
  process (clk) is
  begin
    if rising_edge (clk) then
      cnt <= cnt + STEP;
    end if;
  end process;
  q <= std_logic_vector (cnt);
end rtl;

library ieee;
use ieee.std_logic_1164.all;

entity gentb is
end gentb;

architecture sim of gentb is
  signal clk : std_logic := '0';
  signal q4  : std_logic_vector (3 downto 0);
  signal q12 : std_logic_vector (11 downto 0);
begin
  u_small : entity work.counter
    generic map (WIDTH => 4, STEP => 1) port map (clk => clk, q => q4);
  u_big : entity work.counter
    generic map (WIDTH => 12, STEP => 3) port map (clk => clk, q => q12);
  clk <= not clk after 5 ns;
end sim;
