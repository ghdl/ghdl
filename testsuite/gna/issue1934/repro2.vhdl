package repro2_pkg is
  type inner_t is record
    value : bit_vector;
  end record;

  type inner_array_t is array(natural range<>) of inner_t;

  type outer_t is record
    inner : inner_array_t;
  end record;

  subtype my_type is outer_t(inner(0 to 31)(value(31 downto 0)));
  --  column 44: value
  --  column 35: (0 to 31)
end repro2_pkg;

use work.repro2_pkg.all;

entity repro2 is
end;

architecture behav of repro2 is
begin
  process
    variable v : my_type;
  begin
    assert v.inner'length = 32 severity failure;
    assert v.inner'left = 0 severity failure;
    assert v.inner (0).value'length = 32 severity failure;
    assert v.inner (31).value'right = 0 severity failure;
    wait;
  end process;
end behav;

