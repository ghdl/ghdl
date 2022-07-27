library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro is
  port (o : out unsigned (7 downto 0);
        i : in std_logic_vector(7 downto 0));
end repro;

architecture behav of repro is
begin
  o <= unsigned(i) + 1;
end behav;
