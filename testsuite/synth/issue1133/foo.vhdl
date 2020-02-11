library ieee;
use ieee.std_logic_1164.all;

entity foo is
  port (
    input        : in std_logic_vector(7 downto 0);
    output_ok    : out std_logic_vector(7 downto 0);
    output_error : out std_logic_vector(7 downto 0)
  );
end foo;

architecture foo of foo is

  signal null_vector : std_logic_vector(-1 downto 0) := (others => '0');

begin

  -- This works fine
  null_vector  <= input(null_vector'range);
  output_ok    <= null_vector & (7 downto 0 => '0');
  -- This doesn't
  output_error <= input(-1 downto 0) & (7 downto 0 => '0');

end foo;
