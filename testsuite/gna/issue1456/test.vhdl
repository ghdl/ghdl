package test_package is

  subtype Range1 is natural range 27 downto 0;
  subtype Range2 is natural range Range1'high(0) downto 0;

end test_package;

package body test_package is

end test_package;
