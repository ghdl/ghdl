library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
    port(index : in integer range 0 to 1);
end bug;

architecture behav of bug is

    type foobar is record
        foo : std_logic;
        bar : std_logic_vector(1 downto 0);
    end record;

    -- Changing the order works:
    --type foobar is record
    --    bar : std_logic_vector(1 downto 0);
    --    foo : std_logic;
    --end record;

    type foobar_array is array (0 to 1) of foobar;

    signal s_foobar : foobar_array;
begin
    s_foobar(index).bar(0) <= '0';
end architecture;
