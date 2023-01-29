entity test_tf is
    port(
        a: in bit_vector(3 downto 0);
        red_and: out bit;
        red_nand: out bit;
        red_or: out bit;
        red_nor: out bit;
        red_xor: out bit;
        red_xnor: out bit);
end test_tf;

architecture behavior of test_tf is
begin
  red_and <= and a;
  red_nand <= nand a;
  red_or <= or a;
  red_nor <= nor a;
  red_xor <= xor a;
  red_xnor <= xnor a;
end behavior;
