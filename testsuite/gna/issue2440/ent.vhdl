library IEEE;
use IEEE.std_logic_1164.all;

entity AXI4BFM_Memory is
    generic(
        gLogAddressSpaceWidth : positive := 8
    );
    port(
        somePortsRemoved : in std_logic
    );
end entity AXI4BFM_Memory;

architecture name of AXI4BFM_Memory is
    subtype tLocalAddress is integer range (2**gLogAddressSpaceWidth)-1 downto 0;

    type tMemory is array(tLocalAddress range 1 downto 0) of integer;

    signal m : tMemory;
begin

end architecture name;
