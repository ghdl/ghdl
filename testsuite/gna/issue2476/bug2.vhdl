library IEEE;
use IEEE.std_logic_1164.all;

entity bug_from_2417_fix is
  generic (
    ID_WIDTH : integer := 16
  );
  port (
    aclk     : in    std_logic
  );
end entity;

architecture rtl of bug_from_2417_fix is

  signal reset : std_logic;

  -- Bypass-data FIFO signals:
  type bypass_data_type is record
    resp : std_logic_vector(1 downto 0);
    id   : std_logic_vector(ID_WIDTH-1 downto 0);
    last : std_logic;
  end record;

  signal wbyp_in     : bypass_data_type;
  signal wbyp_out    : bypass_data_type;

begin

  wbypass : if TRUE generate
    subtype ID_RANGE   is natural range ID_WIDTH-1 downto 0;
    subtype RESP_RANGE is natural range ID_WIDTH+1 downto ID_WIDTH;
    constant LAST_IDX : natural := ID_WIDTH+2;
    signal data_in  : std_logic_vector(LAST_IDX downto 0);
    signal data_out : std_logic_vector(LAST_IDX downto 0);
  begin
    data_in(ID_RANGE)   <= wbyp_in.id;
    data_in(RESP_RANGE) <= wbyp_in.resp;
    data_in(LAST_IDX)   <= wbyp_in.last;

    wbyp_out.id   <= data_out(ID_RANGE);
    wbyp_out.resp <= data_out(RESP_RANGE);
    wbyp_out.last <= data_out(LAST_IDX);
  end generate;

end architecture;
