library ieee;
context ieee.ieee_std_context;
use work.uCPUtypes.all;

entity ROM is
  port (
    abus : in  unsigned_byte;
    dbus : out code_word;
    en   : in  logic
  );
end entity ROM;

architecture RTL of ROM is
  type memory is array (0 to 255) of code_word;
  constant mem : memory := (x"777", others => x"000");
begin

dbus <= mem(to_integer(abus)) when en else x"ZZZ";

end architecture RTL;

