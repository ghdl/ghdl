package defs is
    type t_state is record
        a: integer range 0 to 1;
        b: integer range 0 to 0;
        c: integer range 0 to 1;
    end record;
end package;

library ieee;
use ieee.std_logic_1164.all;

entity top_level is
    port (
        clk: in std_ulogic;
        i_state: in work.defs.t_state;
        o_state: out work.defs.t_state
    );
end entity;

architecture arch of top_level is
begin
    o_state <= i_state;
end architecture;
