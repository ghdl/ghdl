--  Same conversion, on the port map of a block header: that one has no
--  instance to hold the layout of the slice.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.finish;

entity blk is
  generic (w : positive := 4);
end blk;

architecture behav of blk is
  signal s : std_ulogic_vector (15 downto 0);
begin
  b : block
    port (p : out unsigned (w - 1 downto 0));
    port map (std_ulogic_vector(p) => s (w - 1 downto 0));
  begin
    p <= (others => '1');
  end block;

  process
  begin
    wait for 1 ns;
    assert s (3 downto 0) = "1111"
      report "bad s: " & to_string (s) severity failure;
    finish;
  end process;
end architecture;
