library ieee;
use ieee.std_logic_1164.all;

entity test_entity is
    generic(
        DO_GEN : boolean := false
        );
    port(
        val_out : out std_logic
        );
end test_entity;

architecture rtl of test_entity is
begin
    set_val_1: if DO_GEN generate
        val_out <= '1';
    end generate;

    set_val_0: if not DO_GEN generate
        val_out <= '0';
    end generate;
end;
