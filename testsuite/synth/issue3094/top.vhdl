library ieee;
use ieee.std_logic_1164.all;

entity top is
end entity;

architecture topa of top is
   signal a, b, y : std_logic;
   signal x : std_logic_vector(1 downto 0);
begin
   W_A : entity work.wrapperA(structural)
   port map(a => a, b => b);
   W_B : entity work.wrapperB(structural)
   port map(x => x, y => y);

end architecture;
