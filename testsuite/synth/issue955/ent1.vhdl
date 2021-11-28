library ieee;
use ieee.std_logic_1164.all;

entity ent1 is
    port (
        clk : in std_logic;
        o : out std_logic_vector (0 to 7)
    );
end ent1;

architecture a of ent1 is
    type reg_t is array(0 to 7) of std_logic_vector(0 to 7);

    signal reg1 : reg_t := (x"10", x"11", x"12", x"13",
                           x"14", x"15", x"16", x"17");
begin
    process(clk)
    begin
        if rising_edge(clk) then
            reg1 <= reg1(1 to 7) & x"00";
        end if;
    end process;

    o <= reg1 (0);
end;
