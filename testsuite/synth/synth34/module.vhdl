library ieee;
  use ieee.std_logic_1164.all;

entity module is
  port (
    clk : in std_logic;
    a : in std_logic_vector(7 downto 0);
    b : out std_logic_vector(7 downto 0)
  );
end module;

architecture rtl of module is
begin
  i_submodule : entity work.submodule
  port map (
    clk => clk,
    a => a,
    b => b
  );
end rtl;
