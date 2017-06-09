library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

entity foo is
  port (
    a : std_logic;
    b : std_logic_vector(7 downto 0)
  );
end entity foo;

architecture RTL of foo is

  signal s_test : std_logic_vector(3 downto 0) := "1111";

begin

  cmp_bar: entity work.bar
    port map(
      a => a
    );
    
  gen_bars: for i in 0 to 1 generate
    cmp_generated_bar: entity work.bar
      port map(
        a => a
      );
  end generate gen_bars;

end architecture;
