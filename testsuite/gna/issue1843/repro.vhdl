ENTITY repro IS
  GENERIC(WIDTH : positive := 32);
  PORT(clk        : IN     bit);
END;

ARCHITECTURE rtl OF repro IS
  CONSTANT UPPER_LIMIT : bit_vector(WIDTH DOWNTO 0) := (OTHERS => '1');
  SIGNAL count : bit_vector((WIDTH-1) DOWNTO 0);
BEGIN

  p_counter : PROCESS (clk)
  BEGIN
    IF ('0', count) /= UPPER_LIMIT then
      count(0) <= '1';
    END IF;
  END PROCESS;

END ARCHITECTURE rtl;
