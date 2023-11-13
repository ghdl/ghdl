LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY test IS
  GENERIC(
    ADDR_WIDTH      : positive := 32;
    MASTER_ID_WIDTH : positive :=  4
  );
END ENTITY;

ARCHITECTURE rtl OF test IS

  CONSTANT MASTER_IDX_WIDTH : positive := 2;
  CONSTANT SLAVE_ID_WIDTH   : positive := MASTER_ID_WIDTH+MASTER_IDX_WIDTH;

  FUNCTION to_slv(awid, awaddr: std_logic_vector) RETURN std_logic_vector IS
  BEGIN
    RETURN (awid(SLAVE_ID_WIDTH-1 DOWNTO 0), awaddr(ADDR_WIDTH-1 DOWNTO 0));
  END FUNCTION;

BEGIN
  process
    variable awid : std_logic_vector(7 downto 0) := x"ba";
    variable awaddr : std_logic_Vector(31 downto 0) := x"f012_345e";
    constant c : std_logic_vector := to_slv(awid, awaddr);
  begin
    assert c = 38x"3a_f012_345e" severity failure;
    report to_hstring(c);
    wait;
  end process;
END ARCHITECTURE;
