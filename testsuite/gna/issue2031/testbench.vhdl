library Bugtests;
use Bugtests.abc.all;

library IEEE;
use IEEE.std_logic_1164.all;

entity test is

end entity;

architecture arch_test of test is 
    signal tb : std_logic;
begin
    DUT: entity Bugtests.abc
    port map(
        a => '1',
        b => '0',
        c => tb
    );
end architecture;
