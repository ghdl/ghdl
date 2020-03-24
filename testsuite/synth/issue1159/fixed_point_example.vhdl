library ieee;
  use ieee.std_logic_1164.all;
  use ieee.fixed_pkg.all;
  use ieee.fixed_float_types.all;

entity fixed_point_example is
  port (
    data_in  : in std_logic_vector(7 downto 0);
    data_out : out std_logic_vector(7 downto 0)
  );
end fixed_point_example;

architecture behavioral of fixed_point_example is
begin
  data_out <= to_slv(to_sfixed(data_in, 7, 0));
end behavioral;
