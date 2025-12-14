library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity externalerr05_sub is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr05_sub;

architecture behav of externalerr05_sub is
  constant accum : std_logic_vector(7 downto 0) := x"05";
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

entity externalerr05 is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        accum : out std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr05;

architecture behav of externalerr05 is
  component externalerr05_sub is
    port (
      clk :     std_logic;
      rst :     std_logic;
      a   :     std_logic_vector(7 downto 0);
      o   : out std_logic_vector(7 downto 0));
  end component externalerr05_sub;
begin
  dut : entity work.externalerr05_sub
    port map (clk => clk,
              rst => rst,
              a => a,
              o => o);
  accum <= << signal .externalerr05.dut.accum : std_logic_vector(7 downto 0) >>;
end behav;
