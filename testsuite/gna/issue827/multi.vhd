--  Two instances of the same architecture, with a different generic: the
--  layout of the slice actual must be per-instance, not shared.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sub is
  generic (w : positive);
  port (o : out unsigned (w - 1 downto 0));
end sub;

architecture behav of sub is
begin
  o <= (others => '1');
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top is
  generic (w : positive);
  port (q : out std_ulogic_vector (15 downto 0));
end top;

architecture behav of top is
begin
  inst : entity work.sub
    generic map (w => w)
    port map (std_ulogic_vector(o) => q (w - 1 downto 0));
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use std.env.finish;

entity multi is
end multi;

architecture behav of multi is
  signal q1 : std_ulogic_vector (15 downto 0);
  signal q2 : std_ulogic_vector (15 downto 0);
begin
  t1 : entity work.top generic map (w => 4) port map (q => q1);
  t2 : entity work.top generic map (w => 12) port map (q => q2);

  process
  begin
    wait for 1 ns;
    assert q1 (3 downto 0) = "1111"
      report "bad q1: " & to_string (q1) severity failure;
    assert q2 (11 downto 0) = "111111111111"
      report "bad q2: " & to_string (q2) severity failure;
    finish;
  end process;
end architecture;
