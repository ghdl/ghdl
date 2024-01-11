library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity test_ent is
  port (
    vec   : in  std_logic_vector
  );
end test_ent;

architecture rtl of test_ent is
begin
end rtl;

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity test is
end test;

architecture rtl of test is
  constant a : std_logic_vector(2 downto 0) := "010";
  constant b : std_logic_vector(2 downto 0) := "101";
begin

  inst : entity work.test_ent
    port map(
      vec   => a(0) & b(1)
    );

end rtl;
