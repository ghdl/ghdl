entity ex1_entity is
  port (
    X : inout boolean            -- std_logic works
  );
end entity;

architecture a of ex1_entity is
begin
end architecture;

library IEEE;
use     IEEE.std_logic_1164.all;

entity ex1_top is
end entity;

architecture a of ex1_top is
  signal A : std_logic;
begin
  inst : entity work.ex1_entity
    port map (
      X => A               -- line containing error
    );
end architecture;
