--
-- Dual-Port Block RAM with Two Write Ports
-- Modelization with a protected shared variable
-- Simulates without warning in VHDL-2002 simulators
--
-- Download: ftp://ftp.xilinx.com/pub/documentation/misc/xstug_examples.zip
-- File: HDL_Coding_Techniques/rams/ram_protected_sharedvar.vhd
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

package ram_pkg is

  subtype data_type is std_logic_vector(15 downto 0);
  
  type ram_type is protected
   
    procedure write (
      addr : std_logic_vector(6 downto 0);
      data : data_type);  
  
    impure function read (
      addr : std_logic_vector(6 downto 0))
      return data_type;

  end protected ram_type;

end ram_pkg;


package body ram_pkg is

  type ram_array is array(0 to 127) of data_type;
  
  type ram_type is protected body
  
    variable ram : ram_array;
    
    procedure write (
      addr : std_logic_vector(6 downto 0);
      data : data_type) is
    begin
      ram(conv_integer(addr)) := data;
    end procedure write;

    impure function read (
      addr : std_logic_vector(6 downto 0))
      return data_type is
    begin
      return ram(conv_integer(addr));
    end function read;

  end protected body ram_type;
  
end ram_pkg;


library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library work;
use work.ram_pkg.all;


entity ram_protected_sharedvar is

  generic (
    DATA_WIDTH : integer := 16;
    ADDR_WIDTH : integer := 7
  );
  port(
    clka  : in  std_logic;
    clkb  : in  std_logic;
    ena   : in  std_logic;
    enb   : in  std_logic;
    wea   : in  std_logic;
    web   : in  std_logic;
    addra : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
    addrb : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
    dia   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    dib   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    doa   : out std_logic_vector(DATA_WIDTH-1 downto 0);
    dob   : out std_logic_vector(DATA_WIDTH-1 downto 0));
    
end ram_protected_sharedvar;

architecture behavioral of ram_protected_sharedvar is

    shared variable RAM : ram_type;
    
begin

    process (CLKA)
    begin
        if rising_edge(clka) then
            if ENA = '1' then
                doa <= RAM.read(addra);
                if WEA = '1' then
                  RAM.write(addra, dia);
                end if;
            end if;
        end if;
    end process;

    process (CLKB)
    begin
        if rising_edge(clkb) then
            if ENB = '1' then
                dob <= RAM.read(addrb);
                if WEB = '1' then
                  RAM.write(addrb, dib);
                end if;
            end if;
        end if;
    end process;

end behavioral;
