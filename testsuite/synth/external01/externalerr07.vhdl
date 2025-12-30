library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity externalerr07_sub is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr07_sub;

architecture behav of externalerr07_sub is
  signal accum : std_logic_vector(7 downto 0);
  signal r : real range 0.0 to real'high;
begin
  process (clk) is
  begin
    if rising_edge(clk) then
      if rst = '1' then
        accum <= (others => '0');
      else
        accum <= std_logic_vector(unsigned(accum) + unsigned(a));
      end if;
    end if;
  end process;

  process (clk) is
  begin
    if rising_edge(clk) then
      if rst = '1' then
        o <= (others => '0');
      else
        o <= accum;
      end if;
    end if;
  end process;
end behav;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity externalerr07 is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        accum : out std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr07;

architecture behav of externalerr07 is
  signal r2: real;
begin
  dut : entity work.externalerr07_sub
    port map (clk => clk,
              rst => rst,
              a => a,
              o => o);
  accum <= << signal dut.accum : std_logic_vector(7 downto 0) >>;
  r2 <= << signal dut.r : real >>;
end behav;
