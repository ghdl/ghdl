library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.textio.all;

--use work.GENERIC_WHEN.all;

entity TEST2 is
begin
end entity TEST2;

architecture BEHAVIOUR of TEST2 is
        component GENERIC_WHEN is
                generic( FOO  :     std_logic_vector(1 downto 0) );
                port(    IN1  : in  std_logic_vector(1 downto 0);
                         OUT1 : out std_logic_vector(1 downto 0) );
        end component GENERIC_WHEN;

        signal S1 : std_logic_vector(1 downto 0);
        signal S2 : std_logic_vector(1 downto 0);
begin

        GENERIC_WHEN_INST : GENERIC_WHEN
                generic map ( FOO  => "0" & "0")
                port map (    IN1  => S1,
                              OUT1 => S2 );
        process
                variable l : line;
        begin
                S1 <= "01";
                writeline(output, l);
                wait;
        end process;

end architecture BEHAVIOUR;

