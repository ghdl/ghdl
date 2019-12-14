library ieee;
use ieee.std_logic_1164.all;

package a is
    generic(
        size: positive
    );
    subtype t is std_logic_vector(size-1 downto 0);
end package a;


entity test is
end test;

architecture dataflow of test is
    package p is new work.a generic map(
        size => 3
    );
begin
    entities:
    for i in 0 to 7 generate
         signal a: work.a.t;
    begin
    end generate entities;
end dataflow;
