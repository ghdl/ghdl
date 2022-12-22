library ieee;
use ieee.std_logic_1164.all;

entity aggr1 is
    generic(
        DATA_WIDTH : integer := 8
        );
    port (a : out std_logic_vector(DATA_WIDTH-1 downto 0);
          b : out std_logic;
          c : std_logic_vector(DATA_WIDTH+1-1 downto 0));
end;

architecture arch of aggr1 is
begin
    (a, b) <= c;
end;

