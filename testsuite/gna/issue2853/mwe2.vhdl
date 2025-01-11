package mwe2_pkg is
    generic (type mwe_t);
end package;

package body mwe2_pkg is
    type mwe_ptr_t is access mwe_t;
end package body;

entity mwe2 is
end entity;

architecture test of mwe2 is
begin
    process is
        type test_t is record
            bv : bit_vector;
        end record;
        package test_pkg is new work.mwe2_pkg generic map (mwe_t => test_t);
    begin
        std.env.finish;
    end process;
end architecture;
