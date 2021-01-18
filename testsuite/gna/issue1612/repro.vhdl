library ieee;
use ieee.std_logic_1164.all;

entity expose_problem is
  port (
    arg : in std_logic_vector);
end entity;

architecture behav of expose_problem is
begin
end behav;

library ieee;
use ieee.std_logic_1164.all;

entity expose is
  generic (
    ARG : std_logic_vector := x"56");
end entity;

architecture rtl of expose is

begin
  expose_problem_1 : entity work.expose_problem
    port map (
      arg => ARG
      );
end architecture;
