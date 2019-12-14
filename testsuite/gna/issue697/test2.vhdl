library ieee;
use ieee.std_logic_1164.all;

package a2 is
    generic(
        size: positive
    );
    subtype t is std_logic_vector(size-1 downto 0);
end package a2;


entity test2 is
end test2;

architecture dataflow of test2 is
    package p is new work.a2 generic map(
        size => 3
    );
begin
    entities:
    for i in 0 to 7 generate
         signal a: p.t;
    begin
    end generate entities;
end dataflow;
