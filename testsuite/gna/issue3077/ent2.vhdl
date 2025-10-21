library ieee;
use ieee.std_logic_1164.all;

--hdlregression:tb
entity test is
end test;

architecture beh of test is
  type t_data is record
    adc_data : std_logic_vector(7 downto 0);
  end record t_data;
  signal data : t_data;
  signal sig : data.adc_data'subtype;
  subtype t_type is data.adc_data'subtype;
begin
  process is
  begin
    sig <= x"00";
    report "end-test";
    wait;
  end process;
end architecture;
