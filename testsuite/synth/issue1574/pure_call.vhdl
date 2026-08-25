library ieee;
use ieee.std_logic_1164.all;

entity pure_call is
    port (
        clock : in std_logic;
        output : out std_logic_vector(7 downto 0)
    );
end pure_call;

architecture bug_arch of pure_call is
    signal input : std_logic := '0';

    impure function read_input return std_logic_vector is
    begin
        if input = '1' then
            return x"A0";
        else
            return x"00";
        end if;
    end read_input;

    --  Declared pure, but calls an impure function: the pure rule is
    --  violated here, not in the body of this function.
    pure function my_func return std_logic_vector is
    begin
        return read_input;
    end my_func;
begin
    process (clock)
    begin
        if rising_edge(clock) then
            output <= my_func;
        end if;
    end process;
end bug_arch;
