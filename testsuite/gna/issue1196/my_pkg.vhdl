library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

package my_pkg is

  type t_frame_x record is
    a : std_logic_vector(1 downto 0);
    b : std_logic_vector(17 downto 0);
  end record t_frame_x;

end package my_pkg;
