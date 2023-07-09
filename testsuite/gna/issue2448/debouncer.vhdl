-----------------------------------------------------------------------------------------------------------------------------------------------------
-- 
--    design:             debouncer
-- 
--    target device:       Intel FPGA's
--    creation date:       01.03.21
--    VHDL standard:       2008
--
--    author:              Sebastian Schmitz
--    copyright:           Lumino Licht Elektronik GmbH
--
--    revision history:    
--
--
-----------------------------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity debouncer is
    generic(
        DEBOUNCE_LENGTH : natural := 63);
    port(
        clk_i   : in  std_logic;
        reset_i : in  std_logic;
        key_i   : in  std_logic;
        key_o   : out std_logic);
end entity;

architecture behavior of debouncer is

    signal key_debounced : std_logic                              := '0';
    signal debounce_cnt  : integer range 0 to DEBOUNCE_LENGTH + 1 := 0;

begin

    process(clk_i, reset_i)
    begin

        if (reset_i = '1') then
            key_debounced <= '0';
            debounce_cnt  <= 0;
            key_o         <= '0';

        elsif (rising_edge(clk_i)) then

            -- XOR
            if (key_i = key_debounced) then
                debounce_cnt <= 0;
            else
                debounce_cnt <= debounce_cnt + 1;
            end if;

            -- Latch
            if (debounce_cnt = DEBOUNCE_LENGTH) then
                key_debounced <= key_i;
                debounce_cnt  <= 0;
            end if;

            key_o <= key_debounced;
        end if;

    end process;

end architecture;

