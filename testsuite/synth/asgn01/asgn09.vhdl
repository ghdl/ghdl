library ieee;
use ieee.std_logic_1164.all;

entity asgn09 is
  port (a, b, c, d : std_logic_vector (1 downto 0);
        sel : std_logic_vector(1 downto 0);
        o : out std_logic_vector (3 downto 0));
end asgn09;

architecture behav of asgn09 is
begin
  with sel select
    o (1 downto 0) <= a when "00",
    b when "01",
    c when "10",
    d when others;
  o(3 downto 2) <= a or b;
end behav;
