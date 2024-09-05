ENTITY repro2 IS
END;

ARCHITECTURE behave OF repro2 is
  signal data  : bit_VECTOR(31 DOWNTO 0);
  function func return bit_vector is
  begin
    return (9 downto 3 => '1');
  end;
BEGIN
  process
  begin
    data <= (func, OTHERS => '0');
    wait for 1 ns;
    assert data (31) = '1' severity failure;
    assert data (25) = '1' severity failure;
    assert data (24) = '0' severity failure;
    wait;
  end process;
END ARCHITECTURE behave;
