library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;
entity nfmac is generic(
    variable_name            :integer:=0
    );
port(          
    port_name:		    in std_logic         
    );
end nfmac;
architecture rtl of nfmac is
begin    
    rxfifogen:if (variable_name = 1) generate
    else generate  
    end generate rxfifogen;
end rtl; 
