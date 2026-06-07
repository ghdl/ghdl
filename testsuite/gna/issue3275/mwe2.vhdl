package mwe2_pkg is
    type bv_vec is array (natural range <>) of bit_vector;

    type mwe_t is record
        vec : bv_vec(open)(7 downto 0);  -- does not work
        -- vec : bv_vec(open)(open);     -- works
    end record;
    type mwe_ptr_t is access mwe_t;
end package;

--------------------------------------------------------------------------------

library work;
use work.mwe2_pkg.all;

entity mwe2 is
end entity;

architecture rtl of mwe2 is
begin
  process
    variable p : mwe_ptr_t := new mwe_t'(vec => (0 => x"01", 1 => x"02"));
    constant l : natural := p.vec'length;
  begin
    assert p.vec(0) = x"01";
    assert l = 2;
    wait;
  end process;
end architecture;
