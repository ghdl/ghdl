library ieee;

use ieee.std_logic_1164.all;

entity e1 is

port(

r1: in real;

slv1: in std_logic_vector(7 downto 0);

sl1: in std_logic

);

end;

architecture a of e1 is

begin

end;

library ieee;
use ieee.std_logic_1164.all;

entity e2 is
begin
end;

architecture a of e2 is
constant r2: integer := 10e6;

signal slv2: std_logic_vector(7 downto 0);
signal sl2: std_logic;
begin
tx: entity work.e1
port map(
r1 => real(r2_wrong),
slv1 => slv2,
sl1 => sl2
);
end;
