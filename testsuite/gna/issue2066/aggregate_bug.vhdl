library ieee;
use ieee.std_logic_1164.all;

entity aggregate_bug is
end entity aggregate_bug;

architecture rtl of aggregate_bug is
  signal vec : std_logic_vector(7 downto 0);
begin
  vec <= (3 downto 0 => "111", others => '0'); -- Associate a 3 bit element to a 4 bit slice
    process
      begin
      wait for 1 ns;
      report to_string(vec);
      wait for 1 ns;
      std.env.finish;
    end process;
end architecture rtl;
