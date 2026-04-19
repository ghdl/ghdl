library ieee;
use ieee.std_logic_1164.all;

entity test is
end;

architecture test_arch of test is
  attribute testlabel             : string;
  attribute testlabel of myinst   : label is "hello world";

  component anothermodule is
    port (
      clk : std_logic
      );
  end component;
begin
  myinst : anothermodule port map (
    clk => '1'
    );
end test_arch;
