library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
    generic(
        DATA_WIDTH : integer := 8
    );
end;

architecture arch of ent is
    signal a : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal b : std_logic;
    signal c : std_logic_vector(DATA_WIDTH+1-1 downto 0);
begin
    (a, b) <= c;
end;

