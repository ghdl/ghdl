library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity externalerr06_sub is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr06_sub;

architecture behav of externalerr06_sub is
  signal accum : std_logic_vector(7 downto 0) := x"05";
begin
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

entity externalerr06 is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        accum : out std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr06;

architecture behav of externalerr06 is
  component externalerr06_sub is
    port (
      clk :     std_logic;
      rst :     std_logic;
      a   :     std_logic_vector(7 downto 0);
      o   : out std_logic_vector(7 downto 0));
  end component externalerr06_sub;
begin
  dut : entity work.externalerr06_sub
    port map (clk => clk,
              rst => rst,
              a => a,
              o => o);
  accum <= << variable .externalerr06.dut.accum : std_logic_vector(7 downto 0) >>;
end behav;
