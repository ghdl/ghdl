library ieee;
use ieee.std_logic_1164.all;

library lib_a;

entity wrapperA is
    port (
        a : in  std_logic;
        b : out std_logic
    );
end entity wrapperA;

architecture structural of wrapperA is
begin

    U_A : entity lib_a.Design_Entity(structural)
    port map (do => a, b => b);
    assert(a = '1') severity failure;

end architecture structural;
