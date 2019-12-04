library ieee;
use ieee.std_logic_1164.ALL;

entity child is
  port (
    O1: out std_logic;
    O2: out std_logic
  );
end entity child;

architecture rtl of child is
begin
  O1 <= '0';
  O2 <= '1';
end architecture rtl;


library ieee;
use ieee.std_logic_1164.ALL;

entity top is
  port (
    O: out std_logic
  );
end entity top;

architecture rtl of top is
  component child is
    port (
      O1: out std_logic;
      O2: out std_logic
    );
  end component child;
begin
  inst : child port map(O1 => O);
end architecture rtl;
