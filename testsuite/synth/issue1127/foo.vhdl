library ieee;
use ieee.std_logic_1164.all;

entity foo is
  port (
    addr : in integer;
    nibble0 : out std_logic_vector(3 downto 0);
    nibble1 : out std_logic_vector(3 downto 0)
  );
end foo;

architecture foo of foo is

  type data_array_t is array (3 downto 0) of std_logic_vector(7 downto 0);
  signal data_buffer : data_array_t;

begin

  nibble0 <= data_buffer(addr)(3 downto 0);
  nibble1 <= data_buffer(addr)(7 downto 4);

end foo;
