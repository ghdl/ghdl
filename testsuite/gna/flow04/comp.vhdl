library ieee;
use ieee.std_logic_1164.all;

entity leaf is
  port (a : in std_logic; b : in std_logic; y : out std_logic);
end leaf;

architecture rtl of leaf is
begin
  y <= a and b;
end rtl;

library ieee;
use ieee.std_logic_1164.all;

entity comptop is
  port (i0, i1, i2 : in std_logic; o : out std_logic);
end comptop;

architecture rtl of comptop is
  --  Component declaration + component instantiation (default binding).
  component leaf is
    port (a : in std_logic; b : in std_logic; y : out std_logic);
  end component;
  signal t : std_logic;
begin
  u0 : leaf port map (a => i0, b => i1, y => t);
  u1 : leaf port map (a => t, b => i2, y => o);
end rtl;
