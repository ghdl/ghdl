library ieee;
use ieee.std_logic_1164.all;

entity repro4 is
  port (a, b : std_logic_vector(7 downto 0);
        c : std_logic;
        ena, enb, enc : std_logic;
        o : out std_logic_vector(7 downto 0));
end;

architecture behav of repro4 is
begin
  o <= a when ena = '1' else (others => 'Z');
  o <= b when enb = '1' else (others => 'Z');
  o (7) <= c when enc = '1' else 'Z';
end behav;
