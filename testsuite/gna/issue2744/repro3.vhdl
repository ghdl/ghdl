ENTITY repro3 IS
END;

ARCHITECTURE behave OF repro3 is
  function func return bit_vector is
  begin
    return (9 downto 3 => '1');
  end;
BEGIN
  process
    variable data  : bit_VECTOR(31 DOWNTO 0);
  begin
    data := (func, OTHERS => '0');
    wait;
  end process;
END ARCHITECTURE behave;
