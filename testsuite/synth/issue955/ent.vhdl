library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port (
        clk : in std_logic;
        o : out bit
    );
end ent;

architecture a of ent is
    type reg_t is array(0 to 7) of std_logic_vector(0 to 7);

    signal reg : reg_t;
begin
    process(clk)
    begin
        if rising_edge(clk) then
            reg <= reg(1 to 7) & x"00";
        end if;
    end process;

    o <= '1';
end;
