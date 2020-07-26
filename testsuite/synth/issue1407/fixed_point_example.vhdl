library ieee;
  use ieee.std_logic_1164.all;
  use ieee.fixed_pkg.all;
  use ieee.fixed_float_types.all;

entity fixed_point_example is
  port (
    data_in  : in std_logic_vector(7 downto 0)
  );
end fixed_point_example;

architecture behavioral of fixed_point_example is
  signal data_tmp : sfixed(5 downto -4);
begin
  data_tmp <= resize(
    to_sfixed(data_in, 3, -4),
    data_tmp
  );
end behavioral;
