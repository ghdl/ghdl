library ieee;
use ieee.std_logic_1164.all;

entity foo is
  generic (
    LENGTH : natural
    );
  port (
    input : in std_logic_vector(LENGTH - 1 downto 0)
    );
end foo;

architecture behave of foo is
begin
end behave;

library ieee;
use ieee.std_logic_1164.all;

entity bar is
end entity bar;

architecture behave of bar is
  component foo is
  port (
    input : in std_logic_vector(7 downto 0)
    );
  end component;

begin

  my_foo : foo
    port map (
      input => (others => '0')
      );
end behave;
