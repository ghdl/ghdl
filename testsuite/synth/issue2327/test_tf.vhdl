entity test_tf is
    port(
        a: in bit_vector(7 downto 0);
        e: in bit;
        ea_and: out bit_vector(7 downto 0);
        ae_and: out bit_vector(7 downto 0);
        ea_nand: out bit_vector(7 downto 0);
        ae_nand: out bit_vector(7 downto 0);
        ea_or: out bit_vector(7 downto 0);
        ae_or: out bit_vector(7 downto 0);
        ea_nor: out bit_vector(7 downto 0);
        ae_nor: out bit_vector(7 downto 0);
        ea_xor: out bit_vector(7 downto 0);
        ae_xor: out bit_vector(7 downto 0);
        ea_xnor: out bit_vector(7 downto 0);
        ae_xnor: out bit_vector(7 downto 0));
end test_tf;

architecture behavior of test_tf is
begin
  ea_and <= a and e;
  ae_and <= e and a;

  ea_nand <= a nand e;
  ae_nand <= e nand a;

  ea_or <= a or e;
  ae_or <= e or a;

  ea_nor <= a nor e;
  ae_nor <= e nor a;

  ea_xor <= a xor e;
  ae_xor <= e xor a;

  ea_xnor <= a xnor e;
  ae_xnor <= e xnor a;
end behavior;
