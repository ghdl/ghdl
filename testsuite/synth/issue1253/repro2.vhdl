library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity repro1 is
    port(C, CLR : in std_logic;
         Q : out std_logic_vector(3 downto 0));
end repro1;

architecture archi of repro1 is
    signal tmp: signed(3 downto 0);
begin
    process (C, CLR)
    begin
        if (CLR='1') then
            tmp <= "0000";
        elsif (C'event and C='1') then
            tmp <= 1 + tmp;
        end if;
    end process;

    Q <= std_logic_vector(tmp);

end archi;
