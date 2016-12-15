library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo is
  port (
    a0 : in unsigned(1 downto 0)
    );
end entity;

architecture bar of foo is
begin
  assert a0 = "01";
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo_tb is
  generic
  ( DEFAULT_X : unsigned(1 downto 0) := (others => '0')
  );
end entity;

architecture tb of foo_tb is

  function compute_stuff_with_x(x : unsigned) return unsigned is
  begin
    return x + 1;
  end compute_stuff_with_x;

begin

  foo_inst:
    entity work.foo
    port map
    ( a0 => compute_stuff_with_x(DEFAULT_X)
    );

end architecture;
