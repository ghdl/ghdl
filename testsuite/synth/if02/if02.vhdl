library ieee;
use ieee.std_logic_1164.all;

entity if01 is
  port (a : std_logic;
        b : std_logic;
        sel : std_logic_vector (1 downto 0);
        s : out std_logic);
end if01;

architecture behav of if01 is
begin
  s <= a when sel = "01"
       else b when sel = "10";
end behav;
