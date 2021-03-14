library ieee;
use ieee.std_logic_1164.all;

entity test_case is
  generic(
    stable_count_c : natural := 0;
    data_width_c : integer := 4
    );
  port(
    clock_i    : in std_ulogic;
    data_i     : in std_ulogic_vector(data_width_c-1 downto 0);
    data_o    : out std_ulogic_vector(data_width_c-1 downto 0)
    );
end test_case;

architecture rtl of test_case is
  subtype word_t is std_ulogic_vector(data_width_c-1 downto 0);
  signal cycles_to_go_s : natural range 0 to stable_count_c;
  signal stable_d, last_val : word_t;
begin
  clock: process (clock_i)
  begin
    if rising_edge(clock_i) then
      last_val <= data_i;
      if last_val /= data_i and stable_count_c /= 0 then
        cycles_to_go_s <= stable_count_c - 1;
      elsif cycles_to_go_s = 0 or stable_count_c = 0 then
        stable_d <= data_i;
      else
        cycles_to_go_s <= cycles_to_go_s - 1;
      end if;
    end if;
  end process clock;
    
  data_o <= stable_d;
end rtl;
