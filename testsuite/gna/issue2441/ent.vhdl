library IEEE;
use IEEE.std_logic_1164.all;


entity AnotherBug is
    generic(
        gLogAddressSpaceWidth : positive := 8
    );
    port(
        dummy : in boolean
    );
end entity AnotherBug;

architecture name of AnotherBug is
    subtype tLocalAddress is integer range (2**gLogAddressSpaceWidth)-1 downto 0;

    subtype tDataChunk is std_logic_vector(natural range 64-1 downto 0);
    
    type tMemory is array(tLocalAddress range <>) of tDataChunk;


    signal sMemory : tMemory(tLocalAddress'high downto tLocalAddress'low);

    pure function toMemory(data : std_logic_vector) return tMemory is
        variable result : tMemory(7 downto 0);
    begin
        -- do something

        return result;
    end function;
begin
    
end architecture name;

