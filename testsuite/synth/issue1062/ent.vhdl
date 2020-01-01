library ieee;
use ieee.std_logic_1164.all;

entity ent is
  generic (gen1 : natural;
           genb : boolean := false;
           genv : std_logic_vector(7 downto 0) := "00001111";
           gens : string);
  port (d : out std_logic);
end ent;

architecture behav of ent is
begin
  d <= '1' when gen1 = 5 and genb and (gens = "TRUE")
       else '0';
end behav;
