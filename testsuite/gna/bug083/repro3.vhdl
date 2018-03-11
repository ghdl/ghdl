library ieee;
use ieee.std_logic_1164.all;

entity repro3 is
end;

architecture behav of repro3 is
  type t_axilite_write_address_channel is record
    --DUT inputs
    awaddr  : std_logic_vector;
    awvalid : std_logic;
  end record;

  type t_axilite_write_data_channel is record
    --DUT inputs
    wdata   : std_logic_vector;
    wstrb   : std_logic_vector;
    wvalid  : std_logic;
    --DUT outputs
    wready  : std_logic;
  end record;

  type t_axilite_read_address_channel is record
    --DUT inputs
    araddr  : std_logic_vector;
    arvalid : std_logic;
    arprot  : std_logic_vector(2 downto 0);
    --DUT outputs
    arready : std_logic;
  end record;

  type t_axilite_read_data_channel is record
    --DUT inputs
    rready  : std_logic;
    --DUT outputs
    rdata   : std_logic_vector;
    rresp   : std_logic_vector(1 downto 0);
    rvalid  : std_logic;
  end record;

  type t_axilite_if is record
    write_address_channel  : t_axilite_write_address_channel;
    write_data_channel     : t_axilite_write_data_channel;
    read_address_channel   : t_axilite_read_address_channel;
    read_data_channel      : t_axilite_read_data_channel;
  end record;

  function get_w return natural is
  begin
    return 32;
  end get_w;
begin

  process
    constant addr_width : natural := get_w;
    constant data_width : natural := get_w;

    variable init_if : t_axilite_if
      (  write_address_channel( awaddr( addr_width    -1 downto 0)), 
         write_data_channel(    wdata(  data_width    -1 downto 0),
                                wstrb(( data_width/8) -1 downto 0)),
         read_address_channel(  araddr( addr_width    -1 downto 0)),
         read_data_channel(     rdata(  data_width    -1 downto 0)));
  begin
    wait;
  end process;
end behav;
