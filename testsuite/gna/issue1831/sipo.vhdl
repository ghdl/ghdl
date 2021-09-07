library IEEE;
use IEEE.std_logic_1164.all;

entity SIPO is
end entity SIPO;

architecture RTL of SIPO is
  type slv_array_t is array (natural range <>) of std_logic_vector;
  signal block_reg        : slv_array_t(0 to 3)(7 downto 0);
  
  -- the following works though
  -- type block_t is array (0 to 3) of std_logic_vector(7 downto 0);
  -- signal block_reg        : block_t;
begin
  -- same if in a process
  block_reg <= block_reg(1 to 3) & block_reg(0);
end architecture;
