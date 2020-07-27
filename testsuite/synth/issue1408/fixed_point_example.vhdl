library ieee;
  use ieee.std_logic_1164.all;
  use ieee.fixed_pkg.all;
  use ieee.fixed_float_types.all;

entity fixed_point_example is
end fixed_point_example;

architecture behavioral of fixed_point_example is
  -- from fixed point user guide:
  -- Result of "Signed Reciprocal(A)" has range "-A'right downto -A'left-1".
  -- Result of "Unsigned Reciprocal(A)" has range "-A'right +1 downto -A'left".
  constant C_SIGNED_VALUE : sfixed(7 downto 0) := (others => '0');
  constant C_RECIPROCAL_SIGNED_VALUE : sfixed(0 downto -8) := reciprocal(C_SIGNED_VALUE);
  
  constant C_UNSIGNED_VALUE : ufixed(7 downto 0) := (others => '0');
  constant C_RECIPROCAL_UNSIGNED_VALUE : ufixed(1 downto -7) := reciprocal(C_UNSIGNED_VALUE);
begin
end behavioral;
