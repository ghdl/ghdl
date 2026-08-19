library ieee;
use ieee.std_logic_1164.all;

entity bug is
    port (
        clock : in std_logic;
        output : out std_logic_vector(7 downto 0)
    );
end bug;

architecture bug_arch OF bug is
    type my_enum is (
        thing1,
        thing2,
        thing3
    );

    signal input : my_enum := thing1;

    impure function my_enum_func return std_logic_vector is
    begin
        case input is
            when thing1 => return x"00";
            when others => return x"A0";
        end case;
    end my_enum_func;
begin
    process (clock)
    begin
        if rising_edge(clock) then
            output <= my_enum_func;
        end if;
    end process;
end bug_arch;
