library ieee;
use ieee.std_logic_1164.all;

entity init is
  port (o : out std_logic);
end init;

architecture behav of init is
begin
  o <= '0';
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity forgen02 is
  port (a : out std_logic_vector (7 downto 0));
end;

architecture behav of forgen02 is
--  constant c : std_logic_vector (7 downto 0) := x"a1";
begin
  gen: for i in a'range generate
    inst: entity work.init
      port map (o => a(i));
  end generate;
end behav;
