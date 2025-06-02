library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity toto is
  generic(
    G_RST : std_logic
  );
  port(CLK : in std_logic;
                RST : in std_logic;
                A : out std_logic
  );
 end entity;

architecture rtl of toto is

begin

process(CLK)
begin
  if rising_edge(CLK) then
                if (RST = G_RST) then
                        A <= '0';
                else
                        A <= '1';
                end if;
        end if;
end process;

end rtl;

ghdl -r -gG_RST=1 toto
ghdl:error: override for generic "G_RST" is out of bounds

But changing G_RST type to std_logic_vector is OK :

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity toto is
  generic(
    G_RST : std_logic_vector(0 downto 0)
  );
  port(CLK : in std_logic;
                RST : in std_logic;
                A : out std_logic
  );
 end entity;

architecture rtl of toto is

begin

process(CLK)
begin
  if rising_edge(CLK) then
                if (RST = G_RST(0)) then
                        A <= '0';
                else
                        A <= '1';
                end if;
        end if;
end process;

end rtl;

