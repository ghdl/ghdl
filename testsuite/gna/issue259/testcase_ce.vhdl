library IEEE;
use IEEE.std_logic_1164.all;


entity Testcase_CE is

port (
    CLK          : in std_logic
);
end Testcase_CE;

architecture RTL of Testcase_CE is

    signal y : std_logic; 
    signal x : std_logic;

begin

process (CLK)

begin
    if CLK'event and CLK='1' then

        x <= y when false else '0'; 

    end if;
end process;

end RTL;
