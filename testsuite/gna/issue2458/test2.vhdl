library ieee;
use ieee.std_logic_1164.all;

entity e2 is
  generic (type data_type);
end entity;

architecture e of e2 is
    signal s : data_type;
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity test2 is
end entity;

architecture test of test2 is
begin
    inst : entity work.e2 generic map(data_type => std_logic_vector(7 downto 0));
end architecture;
