library ieee;
use ieee.std_logic_1164.all;
entity test_entity_assert is
  port (
    i_clk : in  std_ulogic;
    o_clk : out std_ulogic
    );
end entity;

library ieee;
use ieee.std_logic_1164.all;

entity simple_configuration_specification_2 is
end entity;

architecture arch of simple_configuration_specification_2 is
  signal s_clk : std_ulogic;

  component test_entity_assert is
    port (
      i_clk : in  std_ulogic;
      o_clk : out std_ulogic
      );
  end component;

  -- the following should be parsed
  for inst_test_entity_assert : test_entity_assert
    use entity work.test_entity_assert;
  end for;
begin

  inst_test_entity_assert : test_entity_assert
    port map (
      i_clk => s_clk,
      o_clk => s_clk
      );

end architecture;
