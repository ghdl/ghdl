package pkg is
  procedure iterate (
    input : in bit_vector);
end pkg;

package body pkg is
  procedure iterate (
    input : in bit_vector) is
    variable j : integer := input'range'left;
  begin  -- iterate
    for i in input'range loop
      assert i = j report "TEST FAILED" severity failure;
      j := j + 1;
    end loop;  -- i in 1 to 10
    assert j = input'range'right + 1 report "TEST FAILED" severity failure;
  end iterate;
end pkg;

entity test is
end test;

architecture only of test is
begin  -- only
  doit: process
  begin  -- process doit
    work.pkg.iterate("0000");
    report "TEST PASSED";
    wait;
  end process doit;
end only;
