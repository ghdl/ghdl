library ieee;
use ieee.std_logic_1164.all,
  ieee.numeric_std.all,
  work.DAC_package.all;

entity DAC_test is

end entity DAC_test;


architecture arch of DAC_test is
  signal CLK                 : std_logic                                := '0';
  signal main_counter        : unsigned(2 + data_size + 5 - 1 downto 0) := (others => '0');
  signal main_counter_max    : unsigned(main_counter'range)             := (others => '1');
  signal data_absolute_value : unsigned(data_size - 1 downto 0);
  signal RST                 : unsigned(20 downto 0)                    := (others => '1');
  -- signal data_serial         : std_logic_vector (5 downto 5);
  -- signal CLK_serial          : std_logic_vector (5 downto 5);
  -- signal transfer_serial     : std_logic_vector (5 downto 5);
  -- signal update_serial       : std_logic_vector (5 downto 5);

begin
  -- GHDL 5 does not accept (xyz'high => '1', others => '0')
  -- claiming xyz'high is not constant
  main_counter_max(main_counter'high)             <= '1';
  main_counter_max(main_counter'high - data_size) <= '1';

  main_proc : process is
  begin
    if main_counter /= main_counter_max then
      RST(RST'high - 1 downto RST'low) <= RST(RST'high downto RST'low+1);
      RST(RST'high)                    <= '0';
      CLK_IF : if CLK = '1' then
        main_counter <= main_counter + 1;

        wait for 100 ns;
      end if CLK_IF;
      CLK <= not CLK;
    else
      wait;
    end if;

    wait;
  end process main_proc;
end architecture arch;


