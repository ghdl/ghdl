package test_pkg is
    type bv_rec_t is record
        bv : bit_vector;
    end record;
end package;

use work.test_pkg.all;

package test_constrained_generic_pkg is
    generic (BV_WIDTH : positive);
    subtype bv_subrec_t is bv_rec_t(bv(BV_WIDTH-1 downto 0));
end package;

package test_constrained_pkg is
  new work.test_constrained_generic_pkg generic map (BV_WIDTH => 4);


use work.test_constrained_pkg.all;

entity test_tb is
end entity;

architecture tb of test_tb is
    signal bv0 : bv_subrec_t;
begin
end architecture;
