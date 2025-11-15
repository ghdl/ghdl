library ieee;
use ieee.std_logic_1164.all;

library lib_b;

entity wrapperB is
    port (
        x : in  std_logic_vector(1 downto 0);
        y : out std_logic
    );
end entity wrapperB;

architecture structural of wrapperB is
begin

   U_B : entity lib_b.Design_Entity(structural)
   port map (x => x, y => y);

end architecture structural;
