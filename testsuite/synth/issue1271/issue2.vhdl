library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port
      (i_foo : in  std_logic;
       o_foo : out std_logic;
       clock : in  std_logic);
end entity issue;

architecture beh of issue is
begin
    process (clock)
        variable v_foo : std_logic := i_foo;
    begin
        -- works without the if
        if rising_edge (clock) then
            v_foo := v_foo xor v_foo;
            o_foo <= v_foo;
        end if;
    end process;
end architecture;
