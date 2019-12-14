library ieee;
use ieee.std_logic_1164.ALL;

entity child is
  port (
    A:  out std_logic
  );
end entity child;

architecture rtl of child is
begin
  A <= '0';
end architecture rtl;


library ieee;
use ieee.std_logic_1164.ALL;

entity top is
  port (
    A:  out std_logic_vector(1 downto 0)
  );
end entity top;

architecture rtl of top is
  component child is
    port (
      A:  out std_logic
    );
  end component child;

  constant N:   integer := 2;
begin
  CHILDGEN: for i in 0 to N-1 generate
  begin
    CHILDINST : child
      port map(
        A => A(i)
      );
  end generate CHILDGEN;
end architecture rtl;
