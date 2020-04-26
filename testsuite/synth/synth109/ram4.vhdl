library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram4 is

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

end ram4;

architecture behavioral of ram4 is
  constant WIDTH : natural := WIDTHB / 4;
  constant SIZE : natural := SIZEB * 4;
  type ramType is array (0 to SIZE-1) of std_logic_vector(WIDTH-1 downto 0);
  shared variable ram : ramType := (others => (others => '0'));
begin
  process (clkB)
  begin
    if rising_edge(clkB) then
      if enB = '1' then 
        if weB = '1' then
          ram(to_integer(unsigned(addrB)&"00")) := diB(WIDTH-1 downto 0);
          ram(to_integer(unsigned(addrB)&"01")) := diB(2*WIDTH-1 downto WIDTH);
          ram(to_integer(unsigned(addrB)&"10")) := diB(3*WIDTH-1 downto 2*WIDTH);
          ram(to_integer(unsigned(addrB)&"11")) := diB(4*WIDTH-1 downto 3*WIDTH);
        end if;
        doB(WIDTH-1 downto 0)         <= ram(to_integer(unsigned(addrB)&"00"));
        doB(2*WIDTH-1 downto WIDTH)   <= ram(to_integer(unsigned(addrB)&"01"));
        doB(3*WIDTH-1 downto 2*WIDTH) <= ram(to_integer(unsigned(addrB)&"10"));
        doB(4*WIDTH-1 downto 3*WIDTH) <= ram(to_integer(unsigned(addrB)&"11"));
      end if;
    end if;
  end process;
end behavioral;
