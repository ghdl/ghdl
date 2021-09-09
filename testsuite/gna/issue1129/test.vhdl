library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ent is
  generic(
    BITS : positive);
  port(
    input : in unsigned(BITS - 1 downto 0));
end entity;

architecture rtl of ent is
begin
end architecture;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
end entity;

architecture rtl of test is
  constant MAX : positive := 7;
  signal input : u_unsigned(MAX - 1 downto 0);
--  constant CONST : unsigned := to_unsigned(MAX, input);
begin
  ent : entity work.ent
    generic map(
      BITS => MAX)
    port map(
      input => to_unsigned(MAX, input));
      --input => CONST);
end architecture;
