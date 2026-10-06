library ieee;
use ieee.std_logic_1164.all;

entity sub is
  port (data : inout std_logic_vector(1 downto 0));
end entity;

architecture a of sub is
begin
  data <= "10";
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity top is
  port (io : inout std_logic_vector(1 downto 0));
end entity;

architecture a of top is
begin
  u : entity work.sub
    port map (data(0) => io(0),
              data(1) => io(1));
end architecture;
