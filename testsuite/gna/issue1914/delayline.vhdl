library IEEE;
use IEEE.std_logic_1164.all;

entity delayline is
    generic (
        delay : positive
    );
    port (
        clk   : in std_logic;
        i     : in std_logic_vector;
        o     : out std_logic_vector
    );
end entity;

architecture rtl of delayline is
    type delay_t is array (natural range <>) of std_logic_vector;
    signal d : delay_t(delay - 1 downto 0)(i'range); -- This statement causes the problem
begin
    assert i'length = o'length
        report "i and o length should be equal"
        severity error;

    (o, d) <= d & i when rising_edge(clk);

end architecture rtl;
