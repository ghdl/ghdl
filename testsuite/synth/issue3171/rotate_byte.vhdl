library ieee;
use ieee.std_logic_1164.all;

entity rotate_byte is

  generic (
    right_not_left : boolean := false);
  port (
    CLK        : in  std_logic;
    the_input  : in  std_logic_vector(7 downto 0);
    the_output : out std_logic_vector(7 downto 0));

end entity rotate_byte;

architecture arch of rotate_byte is
  component rotate_RL is

    generic (
      right_not_left : boolean := false);

    port (
      CLK        : in  std_logic;
      the_input  : in  std_logic_vector;
      the_output : out std_logic_vector);

  end component rotate_RL;

begin  -- architecture arch

  rotate_RL_instanc : rotate_RL
    generic map (
      right_not_left => right_not_left)
    port map (
      CLK        => CLK,
      the_input  => the_input,
      the_output => the_output);

end architecture arch;
