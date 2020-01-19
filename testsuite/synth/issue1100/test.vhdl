library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity test;

architecture behaviour of test is
    type arecordtype is record
        valid : std_ulogic;
        write_data : std_ulogic_vector(63 downto 0);
    end record;

    signal a : arecordtype;

    subtype byte_index_t is unsigned(2 downto 0);
    type permutation_t is array(0 to 7) of byte_index_t;
    signal perm : permutation_t;
    signal data_permuted : std_ulogic_vector(63 downto 0);
begin
    writeback_1: process(all)
        variable j : integer;
    begin
        for i in 0 to 7 loop
            j := to_integer(perm(i)) * 8;
            data_permuted(i * 8 + 7 downto i * 8) <= a.write_data(j + 7 downto j);
        end loop;
    end process;
end;
