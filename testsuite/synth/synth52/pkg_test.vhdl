library work;
  use work.sample_pkg.all;

entity pkg_test is
  port (
    o : out integer
  );
end pkg_test;

architecture rtl of pkg_test is
begin
  o <= SAMPLE_CONSTANT;
end rtl;
