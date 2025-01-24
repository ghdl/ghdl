library ieee;
use ieee.std_logic_1164.all;

entity module_2 is
  port (
    data : inout std_logic
  );
end entity module_2;

architecture behav of module_2 is
begin
end;
    
library ieee;
use ieee.std_logic_1164.all;

entity module_1 is
end entity module_1;

architecture sim of module_1 is
  signal test : std_logic;
begin
  i_module_2 : entity work.module_2
    port map (
      data => <<signal .module_1.test : std_logic>>
    );
end architecture sim;

