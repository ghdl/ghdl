library ieee;
use ieee.std_logic_1164.all;

entity ram2 is

  generic (
    WIDTH      : integer := 32;
    SIZE       : integer := 64;
    ADDRWIDTH  : integer := 6
    );

  port (
    clkA   : in  std_logic;
    clkB   : in  std_logic;
    enA    : in  std_logic;
    enB    : in  std_logic;
    weA    : in  std_logic;
    weB    : in  std_logic;
    addrA  : in  std_logic_vector(ADDRWIDTH-1 downto 0);
    addrB  : in  std_logic_vector(ADDRWIDTH-1 downto 0);
    diA    : in  std_logic_vector(WIDTH-1 downto 0);
    diB    : in  std_logic_vector(WIDTH-1 downto 0);
    doA    : out std_logic_vector(WIDTH-1 downto 0);
    doB    : out std_logic_vector(WIDTH-1 downto 0)
    );

end ram2;

library ieee;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

architecture behavioral of ram2 is
  type ramType is array (0 to SIZE-1) of std_logic_vector(WIDTH-1 downto 0);
  shared variable ram : ramType := (others => (others => '0'));
begin
  process (clkA)
  begin
    if rising_edge(clkA) then
      if enA = '1' then
        if weA = '1' then
          ram(conv_integer(addrA)) := diA;
        end if;
        doA <= ram(conv_integer(addrA));
      end if;
    end if;
  end process;

  process (clkB)
  begin
    if rising_edge(clkB) then
      if enB = '1' then 
        if weB = '1' then
          ram(conv_integer(addrB)) := diB;
        end if;
        doB <= ram(conv_integer(addrB));
      end if;
    end if;
  end process;
end behavioral;
