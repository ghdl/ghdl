library ieee;
use ieee.std_logic_1164.all;

entity sdram is
  port (clk : in std_logic; q0, q1 : inout std_logic);
end entity;

architecture arch of sdram is
  signal q0_reg, q1_reg : std_logic;
begin
  q0_reg <= q0 when rising_edge(clk);
  q0 <= 'Z';
  q1_reg <= q1 when rising_edge(clk);
  q1 <= 'Z';
end architecture arch;

--

library ieee;
use ieee.std_logic_1164.all;

entity top is
  port (
    clk : in std_logic;
    q0 : inout std_logic := 'X'; -- does not work, has buffers
    q1 : inout std_logic         -- this works
  );
end entity;

architecture rtl of top is
begin
  sdram_inst : entity work.sdram port map (clk => clk, q0 => q0, q1 => q1);
end architecture;
