library IEEE;
use IEEE.std_logic_1164.all;

entity delayline1d is
    generic (
        delay : positive
    );
    port (
        clk   : in std_logic;
        i     : in std_logic;
        o     : out std_logic
    );
end entity;

architecture rtl of delayline1d is
    signal d : std_logic_vector(delay - 1 downto 0);
begin
    (o, d) <= d & i when rising_edge(clk);

end architecture rtl;
