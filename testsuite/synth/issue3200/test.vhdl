library ieee;
use ieee.std_logic_1164.all;

entity test is
end;

architecture test_arch of test is
  signal clk                      : std_logic;
  attribute testlabel             : string;
  attribute testlabel of myinst   : label is "hello world";
  attribute testarch              : string;
  attribute testarch of test_arch : architecture is "hello 2";

  attribute testsig : string;
  attribute testsig of clk : signal is "1234";

  component anothermodule is
    port (
      clk : std_logic
      );
  end component;
begin
  myinst : anothermodule port map (
    clk => clk
    );
end test_arch;
