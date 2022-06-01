library ieee;use ieee.std_logic_1164.all;
use ieee.numeric_std;

entity full_adder_tb is
end entity full_adder_tb;

architecture sim of full_adder_tb is
  type rc_data is record
    a : character;
    t:std_logic;
  end record;
  constant e:rc_data:=('0','%');
begin
end architecture sim;
