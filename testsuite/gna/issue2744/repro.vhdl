LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.fixed_pkg.ALL;

ENTITY repro IS
  GENERIC (
    g_COEFF_INT_WIDTH     : NATURAL := 4;
    g_COEFF_FRAC_WIDTH    : NATURAL := 3
  );
  PORT (clk_i                 : IN  STD_LOGIC);
END;

ARCHITECTURE behave OF repro is
  signal sfx: sfixed(g_COEFF_INT_WIDTH-1 DOWNTO -g_COEFF_FRAC_WIDTH);
  signal data  : STD_LOGIC_VECTOR(31 DOWNTO 0);
BEGIN
  pp: PROCESS(clk_i) IS
  BEGIN
    data <= (to_slv(sfx), OTHERS => '0');
  end process;
END ARCHITECTURE behave;
