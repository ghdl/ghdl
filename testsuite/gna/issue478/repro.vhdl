library ieee;
use ieee.std_logic_1164.all;

entity EX_ALUControl is
end entity;

architecture foo of EX_ALUControl is
    signal RST:             std_logic;
    signal RDY:             std_logic;
    signal reentry_guard:   std_logic;
begin

NO_LABEL:
    process (RST, RDY, reentry_guard)
    begin
        report "In EX_ALUControl (RST=" & Std_logic'image(RST) & ", RDY=" & 
        Std_logic'image(RDY) & ", re=" & 
        Std_logic'image(reentry_guard) ")"; -- MISSING AMPERSAND
    end process;
end architecture;
