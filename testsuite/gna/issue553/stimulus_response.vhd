
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library osvvm;
context osvvm.OsvvmContext;


entity Stimulus_Response is
generic ( CLK_PERIOD : Time := 20 ns );
port (
    NRESET          : in std_logic;
    CLK             : in std_logic;

    A   : out std_logic
);
end Stimulus_Response;

architecture Behavioral of Stimulus_Response is

    constant Scrubbing_Test     : False;
    constant MEM_Test           : boolean := False; 

    signal A_int : std_logic;
 
begin

    A   <= A_int;

end Behavioral;
