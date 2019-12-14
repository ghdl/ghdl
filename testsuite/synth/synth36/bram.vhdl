library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity bram is
  generic (
    addr_width : integer := 9;
    data_width : integer := 8
  );
  port (
    clk      : in std_logic;
    we       : in std_logic;
    addr     : in std_logic_vector(addr_width-1 downto 0);
    data_in  : in std_logic_vector(data_width-1 downto 0);
    data_out : out std_logic_vector(data_width-1 downto 0)
  );
end bram;

architecture rtl of bram is
  type mem_type is array (0 to (2**addr_width)-1) of std_logic_vector(data_width-1 downto 0);
  signal mem : mem_type;

begin
  process(clk)
  begin
    if rising_edge(clk) then
      if we = '1' then
        mem(to_integer(unsigned(addr))) <= data_in;
      end if;
    end if;
  end process;
  data_out <= mem(to_integer(unsigned(addr)));
end rtl;
