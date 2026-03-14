library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity test is
  port (
    x : out std_logic;
    a : in std_logic
    );
end entity;

architecture test_arch of test is
begin
  x <= a;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
  port (
    a, b, c : out std_logic
  );
end entity;

architecture tb_arch of tb is
  signal testxy, testab : std_logic;
begin
  genif : if true generate
    genfor : for i in 0 to 1 generate
      test_inst : entity work.test port map (
        x => a,
        a => '1'
        );
    end generate;
  end generate;

  genif2 : if true generate
    test_inst : entity work.test port map (
      x => b,
      a => '0'
      );
  end generate;
  genfor : for i in 0 to 1 generate
    test_inst : entity work.test port map (
      x => c,
      a => '1'
      );
  end generate;
end architecture;
