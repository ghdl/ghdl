library ieee;
use ieee.std_logic_1164.all;

entity mwe_entity is
  port (
    input : in std_ulogic;
    output : out std_ulogic
  );
end entity;

architecture tb of mwe_entity is

begin

  output <= input;

end architecture;






library ieee;
use ieee.std_logic_1164.all;

entity mwe_entity2 is
  port (
    input : in std_ulogic;
    output : out std_ulogic
  );
end entity;

architecture tb of mwe_entity2 is

begin

  output <= not input;

end architecture;






library ieee;
use ieee.std_logic_1164.all;

entity mwe_tb is
end entity;

architecture tb of mwe_tb is

  constant test_boolean : boolean := false;

  signal input, output : std_ulogic;

begin

  ------------------------------------------------------------------------------
  choose_dut : if test_boolean generate

    dut : entity work.mwe_entity
      port map (
        input => input,
        output => output
      );

  else generate

    dut : entity work.mwe_entity2
      port map (
        input => input,
        output => output
      );

  end generate;


end architecture;







library ieee;
use ieee.std_logic_1164.all;

entity mwe_tb2 is
end entity;

architecture tb of mwe_tb2 is

  constant test_boolean : boolean := false;

  signal input, output : std_ulogic;

begin

  ------------------------------------------------------------------------------
  choose_dut : if test_boolean generate

    dut : entity work.mwe_entity
      port map (
        input => input,
        output => output
      );

  else generate

    -- dut : entity work.mwe_entity2
    --   port map (
    --     input => input,
    --     output => output
    --   );

  end generate;

end architecture;








library ieee;
use ieee.std_logic_1164.all;

entity mwe_tb3 is
end entity;

architecture tb of mwe_tb3 is

  constant test_boolean : boolean := true;

  signal input, output : std_ulogic;

begin

  ------------------------------------------------------------------------------
  choose_dut : if test_boolean generate

    dut : entity work.mwe_entity
      port map (
        input => input,
        output => output
      );

  else generate

    dut : entity work.mwe_entity2
      port map (
        input => input,
        output => output
      );

  end generate;

end architecture;











library ieee;
use ieee.std_logic_1164.all;

entity mwe_tb4 is
end entity;

architecture tb of mwe_tb4 is

  constant test_boolean : boolean := true;

  signal input, output : std_ulogic;

begin

  ------------------------------------------------------------------------------
  choose_dut : if test_boolean generate

    -- dut : entity work.mwe_entity
    --   port map (
    --     input => input,
    --     output => output
    --   );

  else generate

    dut : entity work.mwe_entity2
      port map (
        input => input,
        output => output
      );

  end generate;

end architecture;
