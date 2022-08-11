library ieee;
use ieee.std_logic_1164.all;

entity tb is
end;

architecture func of tb is

  type   t_slv_array is array (natural range <>) of std_logic_vector;
  signal slv_array : t_slv_array(0 to 1)(9 downto 0);
  signal slv       : std_logic_vector(9 downto 0);

begin

  p_main : process
  begin

    slv_array <= slv_array(1) & slv;

    std.env.stop;
    wait;  -- to stop completely
  end process p_main;
end architecture func;
