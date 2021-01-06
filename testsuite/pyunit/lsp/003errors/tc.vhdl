library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity tb is
end tb;

architecture behav of tb is
    signal s : std_logic_vector(7 downto 0);
begin
    assert s /= x"73";
end behav;        
