library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram1 is
  generic (
    WIDTHB      : integer := 32;
    SIZEB       : integer := 64;
    ADDRWIDTHB  : integer := 6
    );

  port (
    clkB   : in  std_logic;
    enB    : in  std_logic;
    weB    : in  std_logic;
    addrB  : in  std_logic_vector(ADDRWIDTHB-1 downto 0);
    diB    : in  std_logic_vector(WIDTHB-1 downto 0);
    doB    : out std_logic_vector(WIDTHB-1 downto 0)
    );

end ram1;

architecture behavioral of ram1 is
  type ramType is array (0 to SIZEB-1) of std_logic_vector(WIDTHB-1 downto 0);
  shared variable ram : ramType := (others => (others => '0'));
begin
  process (clkB)
  begin
    if rising_edge(clkB) then
      if enB = '1' then
        if weB = '1' then
          ram(to_integer(unsigned(addrB))) := diB;
        end if;
        doB <= ram(to_integer(unsigned(addrB)));
      end if;
    end if;
  end process;
end behavioral;
