library ieee;
use ieee.std_logic_1164.all;

entity Design_Entity is
    port (
        do : in  std_logic;
        b : out std_logic
    );
end entity Design_Entity;

architecture structural of Design_Entity is
begin
    b <= not do;
end architecture structural;
