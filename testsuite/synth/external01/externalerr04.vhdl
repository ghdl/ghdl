library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity externalerr04_sub is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr04_sub;

architecture behav of externalerr04_sub is
  signal accum : std_logic_vector(7 downto 0);
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

entity externalerr04 is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        accum : out std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr04;

architecture behav of externalerr04 is
  component externalerr04_sub is
    port (
      clk :     std_logic;
      rst :     std_logic;
      a   :     std_logic_vector(7 downto 0);
      o   : out std_logic_vector(7 downto 0));
  end component externalerr04_sub;
begin
  dut : entity work.externalerr04_sub
    port map (clk => clk,
              rst => rst,
              a => a,
              o => o);
  --  Bad kind: not a constant
  accum <= << constant .externalerr04.dut.accum : std_logic_vector(7 downto 0) >>;
end behav;
