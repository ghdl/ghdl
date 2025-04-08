library ieee;
use ieee.std_logic_1164.all;

entity repro1_sub is
  port (a : in std_logic;
        b : out std_logic);
end repro1_sub;

architecture behav of repro1_sub is
begin
  b <= not a;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity repro1 is
  port (a : in std_logic;
        b : out std_logic);
end repro1;

architecture behav of repro1 is
  signal inst_b : std_logic;
begin
  b <= inst_b;

  inst: entity work.repro1_sub
    port map (a, inst_b);
end behav;

