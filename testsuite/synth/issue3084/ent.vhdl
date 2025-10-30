library ieee;
use ieee.std_logic_1164.all;

entity ent is
    port(
        my_in_port : in std_logic;
        my_out_port : out std_logic
    );
end entity;

architecture a of ent is
    signal my_signal : std_logic;
begin
    my_signal <= my_in_port;
    my_out_port <= my_signal;
end a;
