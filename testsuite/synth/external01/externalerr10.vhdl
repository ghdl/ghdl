library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity externalerr10 is
  port (clk : std_logic;
        rst : std_logic;
        a : std_logic_vector(7 downto 0);
        accum : out std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0));
end externalerr10;

architecture behav of externalerr10 is
  component externalerr10_sub is
    port (
      clk :     std_logic;
      rst :     std_logic;
      a   :     std_logic_vector(7 downto 0);
      o   : out std_logic_vector(7 downto 0));
  end component externalerr10_sub;
begin
  dut : externalerr10_sub
    port map (clk => clk,
              rst => rst,
              a => a,
              o => o);
  --  Component not bound
  accum <= << signal dut.accum : std_logic_vector(7 downto 0) >>;
end behav;
