library ieee;
use ieee.std_logic_1164.all;

entity repro3_sub is
  port (a : in std_logic;
        b : out std_logic);
end repro3_sub;

architecture behav of repro3_sub is
begin
  b <= not a;
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity repro3 is
  port (a : in std_logic_vector(1 downto 0);
        b : out std_logic_vector(3 downto 0));
end repro3;

architecture behav of repro3 is
begin
  gen1: for i in a'range generate
    gena: for j in a'range generate
      inst: entity work.repro3_sub
        port map (a(j), b(2*i+j));
    end generate;
  end generate;
end behav;

