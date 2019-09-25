library ieee;
  use ieee.std_logic_1164.all;

entity submodule is
  port (
    clk : in std_logic;
    a : in std_logic_vector(7 downto 0);
    b : out std_logic_vector(7 downto 0)
  );
end submodule;

architecture rtl of submodule is
begin
  process(clk)
  begin
    if rising_edge(clk) then
      b <= a;
    end if;
  end process;
end rtl;
