library ieee;
use ieee.std_logic_1164.all;

entity e is
  generic (type data_type);
end entity;

architecture e of e is
    signal s : data_type;
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity test is
end entity;

architecture test of test is
component e is
  generic (type data_type);
end component;
begin
    inst : e generic map(data_type => std_logic_vector(7 downto 0));
end architecture;
