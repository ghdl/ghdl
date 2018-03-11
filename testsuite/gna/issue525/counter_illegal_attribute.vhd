--
-- <counter.vhd>
--
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
                                

entity counter is

  port 
    (        
      clk       : in  std_logic;
      rst       : in  std_logic;

      data_out  : out std_logic_vector(7 downto 0)
    );

end counter;


architecture arch1 of counter is

  --
  -- NOTE: placing port attributes here in the architecture body is illegal in VHDL,
  --       but unfortunately Vivado requires them here to parse the X_INTERFACE attributes
  --
  attribute X_INTERFACE_INFO      : string;
  attribute X_INTERFACE_PARAMETER : string;

  attribute X_INTERFACE_INFO      of rst : signal is "xilinx.com:signal:reset:1.0 rst RST";
  attribute X_INTERFACE_PARAMETER of rst : signal is "POLARITY ACTIVE_HIGH";

  --
  -- attempt to place attribute on non-existent signal
  --
  attribute X_INTERFACE_PARAMETER of no_such : signal is "POLARITY ACTIVE_HIGH";


  signal count  : unsigned(7 downto 0);

begin

  count <= (others => '0') when rst = '1' else count + 1 when rising_edge(clk);

  data_out <= std_logic_vector(count);

end arch1;

