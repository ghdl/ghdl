library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity external14_sub is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end external14_sub;

architecture behav of external14_sub is
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

entity external14 is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        accum : out std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end external14;

architecture behav of external14 is
begin
  gen: for i in 1 to 1 generate
  dut : entity work.external14_sub
    port map (clk => clk,
              rst => rst,
              a => a,
              o => o);
  end generate;
  accum <= << signal .external14.gen(xx).dut.accum : std_logic_vector(7 downto 0) >>;
end behav;
