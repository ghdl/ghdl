library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_Std.all;

entity casegen1 is
  generic (sel : boolean);
end;

architecture behav of casegen1 is
  signal top : natural := 123;
begin
  g_case: case sel generate
    when false =>
        signal s : bit := '1';
      begin
        s <= '0' after 4 ns;
      end;
    when true =>
        signal s : std_logic;
      begin
        s <= '1' after 3 ns;
      end;
  end generate;
end behav;
