library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.config_pkg.t_config;

entity dutchk is
  port(i_config : in t_config(param_a(31 downto 0), param_b(31 downto 0)));
end entity;

architecture arch of dutchk is
begin
  process
  begin
    wait for 1 ns;
    assert i_config.param_a = std_logic_vector(to_unsigned(34, 32))
      report "FAIL param_a=" & integer'image(to_integer(unsigned(i_config.param_a)))
      severity failure;
    assert i_config.param_b = std_logic_vector(to_unsigned(55, 32))
      report "FAIL param_b=" & integer'image(to_integer(unsigned(i_config.param_b)))
      severity failure;
    report "PASS param_a=" & integer'image(to_integer(unsigned(i_config.param_a))) &
           " param_b=" & integer'image(to_integer(unsigned(i_config.param_b)));
    wait;
  end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.config_pkg.t_config;

entity tb_chk is
end entity;

architecture behaviour of tb_chk is
  constant s_config : t_config(param_a(31 downto 0), param_b(31 downto 0)) :=
    (std_logic_vector(to_unsigned(34,32)), std_logic_vector(to_unsigned(55,32)));
begin
  d : entity work.dutchk port map (i_config => s_config);
end architecture;
