package mwe_pkg is
    type bv_vec is array (natural range <>) of bit_vector;

    type mwe_t is record
        vec : bv_vec(open)(7 downto 0);  -- does not work
        -- vec : bv_vec(open)(open);     -- works
    end record;
    type mwe_ptr_t is access mwe_t;
end package;

--------------------------------------------------------------------------------

library work;
use work.mwe_pkg.all;

entity mwe is
end entity;

architecture rtl of mwe is
begin
end architecture;
