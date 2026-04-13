library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
  port (
    o : out std_logic
  );
end entity;

architecture synthesis of tb is
  signal test : unsigned(1 downto 0); -- maximum is 3
  signal check : std_logic;
begin
  test <= "11";
  check <= '1' when test < 4 else '0'; -- compare against 4...
  o <= check;
end architecture synthesis;

