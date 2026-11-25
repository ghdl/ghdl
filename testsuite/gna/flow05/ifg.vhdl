library ieee;
use ieee.std_logic_1164.all;

entity cell2 is
  port (a : in std_logic; y : out std_logic);
end cell2;

architecture rtl of cell2 is
begin
  y <= not a;
end rtl;

library ieee;
use ieee.std_logic_1164.all;

entity ifgtop is
  generic (USE_INV : boolean := true);
  port (a : in std_logic; y : out std_logic);
end ifgtop;

architecture rtl of ifgtop is
begin
  --  Taken branch: instantiates cell2.
  g_on : if USE_INV generate
    u : entity work.cell2 port map (a => a, y => y);
  end generate;
  --  Pruned branch: must NOT appear in the elaborated hierarchy.
  g_off : if not USE_INV generate
    y <= a;
  end generate;
end rtl;

library ieee;
use ieee.std_logic_1164.all;

entity ifgtb is
end ifgtb;

architecture sim of ifgtb is
  signal a, y : std_logic;
begin
  dut : entity work.ifgtop generic map (USE_INV => true)
    port map (a => a, y => y);
end sim;
