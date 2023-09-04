library IEEE;
use IEEE.std_logic_1164.all;

entity bug_from_2417_fix is
  generic (ID_WIDTH : integer := 16);
end entity;

architecture rtl of bug_from_2417_fix is
  type bypass_data_type is record
    resp : std_logic_vector(1 downto 0);
  end record;

  signal wbyp_in     : bypass_data_type;

  subtype RESP_RANGE is natural range ID_WIDTH+1 downto ID_WIDTH;
  constant LAST_IDX : natural := ID_WIDTH+2;
  signal data_in  : std_logic_vector(LAST_IDX downto 0);
begin
  data_in(RESP_RANGE) <= wbyp_in.resp;
end architecture;
