entity modulo_test is
  port (
    a : in integer;
    b : out integer;
    c : out integer
  );
end modulo_test;

architecture rtl of modulo_test is
begin
  b <= a mod 8;
  c <= a rem 8;
end rtl;
