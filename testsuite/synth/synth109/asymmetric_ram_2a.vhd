--
-- Asymmetric port RAM
--   Port A is 256x8-bit read-and-write (write-first synchronization)
--   Port B is 64x32-bit read-and-write (write-first synchronization)
--
-- Download: ftp://ftp.xilinx.com/pub/documentation/misc/xstug_examples.zip
-- File: HDL_Coding_Techniques/rams/asymmetric_ram_2a.vhd
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

entity asymmetric_ram_2a is

  generic (
    WIDTHA      : integer := 8;
    SIZEA       : integer := 256;
    ADDRWIDTHA  : integer := 8;
    WIDTHB      : integer := 32;
    SIZEB       : integer := 64;
    ADDRWIDTHB  : integer := 6
    );

  port (
    clkA   : in  std_logic;
    clkB   : in  std_logic;
    enA    : in  std_logic;
    enB    : in  std_logic;
    weA    : in  std_logic;
    weB    : in  std_logic;
    addrA  : in  std_logic_vector(ADDRWIDTHA-1 downto 0);
    addrB  : in  std_logic_vector(ADDRWIDTHB-1 downto 0);
    diA    : in  std_logic_vector(WIDTHA-1 downto 0);
    diB    : in  std_logic_vector(WIDTHB-1 downto 0);
    doA    : out std_logic_vector(WIDTHA-1 downto 0);
    doB    : out std_logic_vector(WIDTHB-1 downto 0)
    );

end asymmetric_ram_2a;

architecture behavioral of asymmetric_ram_2a is

  function max(L, R: INTEGER) return INTEGER is
  begin
      if L > R then
          return L;
      else
          return R;
      end if;
  end;


  function min(L, R: INTEGER) return INTEGER is
  begin
      if L < R then
          return L;
      else
          return R;
      end if;
  end;

  constant minWIDTH : integer := min(WIDTHA,WIDTHB);
  constant maxWIDTH : integer := max(WIDTHA,WIDTHB);
  constant maxSIZE  : integer := max(SIZEA,SIZEB);
  constant RATIO : integer := maxWIDTH / minWIDTH;

  type ramType is array (0 to maxSIZE-1) of std_logic_vector(minWIDTH-1 downto 0);
  shared variable ram : ramType := (others => (others => '0'));
  
  signal readA : std_logic_vector(WIDTHA-1 downto 0):= (others => '0');
  signal readB : std_logic_vector(WIDTHB-1 downto 0):= (others => '0');
  signal regA  : std_logic_vector(WIDTHA-1 downto 0):= (others => '0');
  signal regB  : std_logic_vector(WIDTHB-1 downto 0):= (others => '0');

begin

  process (clkA)
  begin
    if rising_edge(clkA) then
      if enA = '1' then
        if weA = '1' then
          ram(conv_integer(addrA)) := diA;
        end if;
        readA <= ram(conv_integer(addrA));
      end if;
      regA <= readA;
    end if;
  end process;

  process (clkB)
  begin
    if rising_edge(clkB) then
      if enB = '1' then 
        if weB = '1' then
          ram(conv_integer(addrB&conv_std_logic_vector(0,2))) := diB(minWIDTH-1 downto 0);
          ram(conv_integer(addrB&conv_std_logic_vector(1,2))) := diB(2*minWIDTH-1 downto minWIDTH);
          ram(conv_integer(addrB&conv_std_logic_vector(2,2))) := diB(3*minWIDTH-1 downto 2*minWIDTH);
          ram(conv_integer(addrB&conv_std_logic_vector(3,2))) := diB(4*minWIDTH-1 downto 3*minWIDTH);
        end if;
        readB(minWIDTH-1 downto 0)            <= ram(conv_integer(addrB&conv_std_logic_vector(0,2)));
        readB(2*minWIDTH-1 downto minWIDTH)   <= ram(conv_integer(addrB&conv_std_logic_vector(1,2)));
        readB(3*minWIDTH-1 downto 2*minWIDTH) <= ram(conv_integer(addrB&conv_std_logic_vector(2,2)));
        readB(4*minWIDTH-1 downto 3*minWIDTH) <= ram(conv_integer(addrB&conv_std_logic_vector(3,2)));
      end if;
      regB <= readB;
    end if;
  end process;

  doA <= regA;
  doB <= regB;
  
end behavioral;
