package pkg is
  generic (
    type T
  );
end package;


use work.pkg.all;

entity test is
end entity;

architecture tb of test is
  package p is new package pkg
    generic map (
      T => integer
    );
begin
end architecture;
