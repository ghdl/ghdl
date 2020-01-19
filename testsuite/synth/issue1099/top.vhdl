library ieee;
use ieee.std_logic_1164.all;

entity has_zero_width_port is
  port(
    width_one_port: out std_logic_vector(0 downto 0);
    width_zero_port: out std_logic_vector(-1 downto 0)
    );
end entity;

architecture arch of has_zero_width_port is
begin
  width_one_port <= (others => '0');
  width_zero_port <= (others => '0');
end architecture;
    
library ieee;
use ieee.std_logic_1164.all;

entity top is
  port(
    width_one_port: out std_logic_vector(0 downto 0);
    width_zero_port: out std_logic_vector(-1 downto 0)
    );
end entity;

architecture arch of top is
begin

  wrapped: entity work.has_zero_width_port
    port map (
      width_one_port => width_one_port,
      width_zero_port => width_zero_port
      );

end architecture;
