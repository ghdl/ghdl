library ieee;
use ieee.std_logic_1164.all;

entity ent1 is
end entity;

architecture a of ent1 is
  signal s1 : std_ulogic_vector(2 downto 0);
  signal s2 : std_ulogic;

  procedure proc (
    signal a : in std_ulogic_vector(2 downto 0)
  ) is
    variable b : std_ulogic;
  begin
    if s2 = '0' then
      b := a(0);
    end if;
  end procedure;

begin
  process (all) is
  begin
    proc(a => s1);
  end process;
end architecture;

library ieee;

entity ent2 is
end entity;

architecture a of ent2 is
begin
  ent1: entity work.ent1;
end a;
