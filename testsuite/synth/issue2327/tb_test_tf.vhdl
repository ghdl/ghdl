entity tb_test_tf is
end tb_test_tf;

architecture behav of tb_test_tf is
  signal a       : bit_vector(7 downto 0);
  signal e       : bit;
  signal ea_and  : bit_vector(7 downto 0);
  signal ae_and  : bit_vector(7 downto 0);
  signal ea_nand : bit_vector(7 downto 0);
  signal ae_nand : bit_vector(7 downto 0);
  signal ea_or   : bit_vector(7 downto 0);
  signal ae_or   : bit_vector(7 downto 0);
  signal ea_nor  : bit_vector(7 downto 0);
  signal ae_nor  : bit_vector(7 downto 0);
  signal ea_xor  : bit_vector(7 downto 0);
  signal ae_xor  : bit_vector(7 downto 0);
  signal ea_xnor : bit_vector(7 downto 0);
  signal ae_xnor : bit_vector(7 downto 0);
begin
  dut: entity work.test_tf
    port map (
      a       => a,
      e       => e,
      ea_and  => ea_and,
      ae_and  => ae_and,
      ea_nand => ea_nand,
      ae_nand => ae_nand,
      ea_or   => ea_or,
      ae_or   => ae_or,
      ea_nor  => ea_nor,
      ae_nor  => ae_nor,
      ea_xor  => ea_xor,
      ae_xor  => ae_xor,
      ea_xnor => ea_xnor,
      ae_xnor => ae_xnor);
  process
  begin
    a <= b"01000111";
    e <= '1';
    wait for 1 ns;

    assert ea_and = b"01000111" severity failure;
    assert ae_and = b"01000111" severity failure;
    assert ea_nand = b"10111000" severity failure;
    assert ae_nand = b"10111000" severity failure;
    assert ea_or = b"11111111" severity failure;
    assert ae_or = b"11111111" severity failure;
    assert ea_nor = b"00000000" severity failure;
    assert ae_nor = b"00000000" severity failure;
    assert ea_xor = b"10111000" severity failure;
    assert ae_xor = b"10111000" severity failure;
    assert ea_xnor = b"01000111" severity failure;
    assert ae_xnor = b"01000111" severity failure;

    e <= '0';
    wait for 1 ns;

    assert ea_and = b"00000000" severity failure;
    assert ae_and = b"00000000" severity failure;
    assert ea_nand = b"11111111" severity failure;
    assert ae_nand = b"11111111" severity failure;
    assert ea_or = b"01000111" severity failure;
    assert ae_or = b"01000111" severity failure;
    assert ea_nor = b"10111000" severity failure;
    assert ae_nor = b"10111000" severity failure;
    assert ea_xor = b"01000111" severity failure;
    assert ae_xor = b"01000111" severity failure;
    assert ea_xnor = b"10111000" severity failure;
    assert ae_xnor = b"10111000" severity failure;
    wait;
  end process;
end behav;
