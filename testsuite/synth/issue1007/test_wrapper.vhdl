library ieee;
use ieee.std_logic_1164.all;

entity test_wrapper is
    port(
        val1_out : out std_logic;
        val2_out : out std_logic
        );
end test_wrapper;

architecture rtl of test_wrapper is
begin
    entity_0 : entity work.test_entity
        generic map (
            DO_GEN => true
            )
        port map (
            val_out => val1_out
            );

    entity_1 : entity work.test_entity
        generic map (
            DO_GEN => false
            )
        port map (
            val_out => val2_out
            );
end;
