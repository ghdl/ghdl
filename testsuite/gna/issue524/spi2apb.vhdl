library IEEE;
use IEEE.std_logic_1164.all;

entity spi2apb is
   port (
      -- SPI
      signal MISO : in  std_logic;
      signal MOSI : out std_logic;
      signal SCLK : out std_logic;
      signal SS_n : out std_logic;

      -- APB
      signal apb_select :  in  std_logic;
      signal clk        :  in  std_logic;
      signal data_in    :  in  std_logic_vector (15 downto 0);
      signal data_out   :  out std_logic_vector (15 downto 0);
      signal addr       :  in  std_logic_vector (2 downto 0);
      signal read_n     :  in  std_logic;
      signal reset_n    :  in  std_logic;
      signal write_n    :  in  std_logic
);
end entity spi2apb;

architecture rtl of spi2apb is
   type reg_type is record
      enabled: std_logic;
      sclk:    std_logic;
      running: std_logic;
      continuous: std_logic;
      pending: std_logic;
      txValid, rxValid: std_logic;
      txBuffer, rxBuffer, shift: std_logic_vector(7 downto 0);
   end record;

   constant reg_reset: reg_type := (
      enabled => '0',
      sclk => '0',
      running => '0',
      continuous => '0',
      pending => '0',
      txValid => '0',
      rxValid => '0',
      txBuffer => (others => '0'),
      shift => (others => '0'),
      rxBuffer => (others => '0')
   );

   signal reg_in, reg_out: reg_type;
begin
   MOSI <= reg_out.shift(7);
   SCLK <= reg_out.sclk;
   SS_n <= not reg_out.enabled;
   data_out <= "000000" & (reg_out.running or reg_out.pending) & reg_out.rxValid & reg_out.rxBuffer;

   sync: process(clk, reset_n)
   begin
      if (rising_edge(clk)) then
         reg_in <= reg_reset when (reset_n => '0') else reg_out;
      end if;
   end process;

   clocking: process(reg_in)
   begin
      if (reg_in.running) then
         reg_out.sclk <= "0" when reg_in else "1";
      end if;
   end process;
end;

-- vim: set sw=3 ts=3 sts=3 et sta sr ai si cin cino=>1s(0u0W1s:
