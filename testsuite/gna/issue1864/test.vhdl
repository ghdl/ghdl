package int_vec_pkg is
  type t_integer_array is array (0 downto 0) of integer_vector(0 downto 0);
  type t_real_array is array (0 downto 0) of real_vector(0 downto 0);
  type t_time_array is array (0 downto 0) of time_vector(0 downto 0);
  signal int_vec : t_integer_array;
  signal real_vec : t_real_array;
  signal time_vec : t_time_array;
end package;
use work.int_vec_pkg.all;

entity test is
end entity test;
architecture beh of test is
begin
  process
  begin
    wait;
  end process;
end architecture beh;
