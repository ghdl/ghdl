library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.ALL;

entity range_filter is
    generic(
        G_WIDTH              :   natural := 29
    );
    port(
        filter_input       : in    std_logic_vector(G_WIDTH - 1 downto 0);
        valid              : out   std_logic
    );
end entity;

architecture rtl of range_filter is
    procedure to_decimal(
        signal res   : out natural
    ) is
    begin
        res <= 4;
    end procedure to_decimal;

    signal value_dec                :   natural range 0 to (2 ** G_WIDTH - 1);
begin
    to_decimal(value_dec);

    valid  <= '1' when value_dec <= 12 else '0';
end architecture;
