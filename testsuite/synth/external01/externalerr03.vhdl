library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity externalerr03_sub is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr03_sub;

architecture behav of externalerr03_sub is
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

entity externalerr03 is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        accum : out std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr03;

architecture behav of externalerr03 is
  component externalerr03_sub is
    port (
      clk :     std_logic;
      rst :     std_logic;
      a   :     std_logic_vector(7 downto 0);
      o   : out std_logic_vector(7 downto 0));
  end component externalerr03_sub;
begin
  dut : entity work.externalerr03_sub
    port map (clk => clk,
              rst => rst,
              a => a,
              o => o);
  --  Bad name: dut is an instance.
  accum <= << signal .externalerr03.dut : std_logic_vector(7 downto 0) >>;
end behav;
