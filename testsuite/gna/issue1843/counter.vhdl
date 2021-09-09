LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY counter IS
  GENERIC(
    WIDTH : positive := 32
  );
  PORT( 
    clk        : IN     std_logic
  );
END ENTITY counter;

ARCHITECTURE rtl OF counter IS
  CONSTANT UPPER_LIMIT : unsigned((WIDTH-1) DOWNTO 0) := (OTHERS => '1');
  SIGNAL count : unsigned((WIDTH-1) DOWNTO 0);
BEGIN

  p_counter : PROCESS (clk)
  BEGIN
    IF (rising_edge(clk)) THEN
      IF ((('0', count) + 1) < UPPER_LIMIT) THEN
        count <= count + 1;
      END IF;
    END IF;
  END PROCESS;

END ARCHITECTURE rtl;
