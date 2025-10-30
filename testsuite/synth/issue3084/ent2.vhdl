library ieee;
use ieee.std_logic_1164.all;

entity ent2 is
    port(
        my_in_port : in std_logic;
        my_out_port : out std_logic
    );
end entity;

architecture a of ent2 is
begin
    my_out_port <= my_in_port;
end a;
