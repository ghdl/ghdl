library ieee;
use ieee.std_logic_1164.all;

entity sdram is
  port (clk : in std_logic; q0: in std_logic; q1 : inout std_logic);
end entity;

architecture arch of sdram is
  signal q1_reg : std_logic;
begin
  q1_reg <= q0 when rising_edge(clk);
  q1 <= 'Z';
end architecture arch;

--

library ieee;
use ieee.std_logic_1164.all;

entity top is
  port (
    clk : in std_logic;
    q0 : inout std_logic := '1'; -- does not work, has buffers
    q1 : inout std_logic         -- this works
  );
end entity;

architecture rtl of top is
begin
  sdram_inst : entity work.sdram port map (clk => clk, q0 => q0, q1 => q1);
end architecture;
