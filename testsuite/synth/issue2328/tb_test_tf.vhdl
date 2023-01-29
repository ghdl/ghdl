entity tb_test_tf is
end tb_test_tf;

architecture behav of tb_test_tf is
  signal a        : bit_vector(3 downto 0);
  signal red_and  : bit;
  signal red_nand : bit;
  signal red_or   : bit;
  signal red_nor  : bit;
  signal red_xor  : bit;
  signal red_xnor : bit;
begin
  dut: entity work.test_tf
    port map (
      a        => a,
      red_and  => red_and,
      red_nand => red_nand,
      red_or   => red_or,
      red_nor  => red_nor,
      red_xor  => red_xor,
      red_xnor => red_xnor);
  process
  begin
    a <= b"0101";
    wait for 1 ns;

    assert red_and = '0' severity error;
    assert red_nand = '1' severity error;
    assert red_or = '1' severity error;
    assert red_nor = '0' severity error;
    assert red_xor = '0' severity error;
    assert red_xnor = '1' severity error;

    a <= b"1111";
    wait for 1 ns;

    assert red_and = '1' severity error;
    assert red_nand = '0' severity error;
    assert red_or = '1' severity error;
    assert red_nor = '0' severity error;
    assert red_xor = '0' severity error;
    assert red_xnor = '1' severity error;

    a <= b"0000";
    wait for 1 ns;

    assert red_and = '0' severity error;
    assert red_nand = '1' severity error;
    assert red_or = '0' severity error;
    assert red_nor = '1' severity error;
    assert red_xor = '0' severity error;
    assert red_xnor = '1' severity error;

    a <= b"0001";
    wait for 1 ns;

    assert red_and = '0' severity error;
    assert red_nand = '1' severity error;
    assert red_or = '1' severity error;
    assert red_nor = '0' severity error;
    assert red_xor = '1' severity error;
    assert red_xnor = '0' severity error;

    report "done";
    wait;
  end process;
end behav;
