library IEEE;
use     IEEE.std_logic_1164.all;

entity repro2 is
end entity;

architecture test of repro2 is
 type slvv_t   is array(natural range <>) of std_logic_vector;
 subtype ram_t is slvv_t(0 to 1)(1 downto 0);

 signal ram : ram_t;
 
 alias element is ram_t'element;    -- error because ram_t is not an object
 --alias element_t is ram'element;

 signal ram2 : element;
begin

end architecture; 
