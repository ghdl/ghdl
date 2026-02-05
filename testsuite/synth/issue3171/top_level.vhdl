library ieee;
use ieee.std_logic_1164.all;

entity top_level is
  port (
    CLK         : in  std_logic;
    the_input   : in  std_logic_vector(7 downto 0);
    the_output  : out std_logic_vector(7 downto 0));

end entity top_level;

architecture arch of top_level is
  component rotate_byte is

    generic (
      right_not_left : boolean);
    port (
      CLK        : in  std_logic;
      the_input  : in  std_logic_vector(7 downto 0);
      the_output : out std_logic_vector(7 downto 0));

  end component rotate_byte;

begin  -- architecture arch

  rotate_byte_instanc : rotate_byte
    generic map (
      right_not_left => false)
    port map (
      CLK        => CLK,
      the_input  => the_input,
      the_output => the_output);


end architecture arch;
