library ieee;
use ieee.std_logic_1164.all;

entity crashing_entity is
end crashing_entity;

architecture crashing_arch of crashing_entity is

  type byte_array is array (natural range <>) of std_logic_vector(7 downto 0);
  type message_array is array (natural range <>) of byte_array;

  constant messages : message_array := (
    (X"00", X"00"),
    (X"00", X"00")
  );

begin

end crashing_arch;
