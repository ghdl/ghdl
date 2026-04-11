-- File: test.vhd
library ieee;
use ieee.std_logic_1164.all;

entity inner is
  port (data : in std_logic_vector);
end entity;

architecture rtl of inner is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity top is
end entity;

architecture rtl of top is
  signal a : std_logic_vector(1 downto 0) := "00";
  signal b : std_logic_vector(1 downto 0) := "00";
begin
  u : entity work.inner
    port map (data => a & b);
end architecture;
