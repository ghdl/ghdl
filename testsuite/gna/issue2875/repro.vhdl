library ieee;
use ieee.std_logic_1164.all;

entity repro is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture struct of repro is
    component comp is
    end component;
begin
    process
    begin
        report "path-name = " & comp'path_name ;
        wait;
    end process;
end architecture;
