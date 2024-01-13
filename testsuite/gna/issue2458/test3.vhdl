library ieee;
use ieee.std_logic_1164.all;

entity e3 is
  generic (type data_type);
end entity;

architecture e of e3 is
    signal s : data_type;
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity test3 is
end entity;

architecture test of test3 is
component e3 is
  generic (type data_type);
end component;
begin
    inst : e3 generic map(data_type => std_logic);
end architecture;
