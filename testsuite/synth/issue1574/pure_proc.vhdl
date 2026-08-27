library ieee;
use ieee.std_logic_1164.all;

entity pure_proc is
    port (
        clock : in std_logic;
        output : out std_logic_vector(7 downto 0)
    );
end pure_proc;

architecture bug_arch of pure_proc is
    signal input : std_logic := '0';

    procedure read_input (v : out std_logic_vector(7 downto 0)) is
    begin
        if input = '1' then
            v := x"A0";
        else
            v := x"00";
        end if;
    end read_input;

    --  Declared pure, but calls a procedure that reads a signal of the
    --  enclosing architecture.
    pure function my_func return std_logic_vector is
        variable r : std_logic_vector(7 downto 0);
    begin
        read_input (r);
        return r;
    end my_func;
begin
    process (clock)
    begin
        if rising_edge(clock) then
            output <= my_func;
        end if;
    end process;
end bug_arch;
