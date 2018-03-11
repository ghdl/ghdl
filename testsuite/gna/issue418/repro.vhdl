entity repro is
end;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of repro is
   -- AXI-Lite Interface signals    
  type t_axilite_write_address_channel is record
    --DUT inputs
    awaddr  : std_logic_vector;
    awvalid : std_logic;
    awprot  : std_logic_vector(2 downto 0); -- [0: '0' - unpriviliged access, '1' - priviliged access; 1: '0' - secure access, '1' - non-secure access, 2: '0' - Data access, '1' - Instruction accesss]
    --DUT outputs
    awready : std_logic;
  end record;

  type t_axilite_write_data_channel is record
    --DUT inputs
    wdata   : std_logic_vector;
    wstrb   : std_logic_vector;
    wvalid  : std_logic;
    --DUT outputs
    wready  : std_logic;
  end record;

  type t_axilite_write_response_channel is record
    --DUT inputs
    bready  : std_logic;
    --DUT outputs
    bresp   : std_logic_vector(1 downto 0);
    bvalid  : std_logic;
  end record;

  type t_axilite_read_address_channel is record
    --DUT inputs
    araddr  : std_logic_vector;
    arvalid : std_logic;
    arprot  : std_logic_vector(2 downto 0);  -- [0: '0' - unpriviliged access, '1' - priviliged access; 1: '0' - secure access, '1' - non-secure access, 2: '0' - Data access, '1' - Instruction accesss]
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
    write_response_channel : t_axilite_write_response_channel;
    read_address_channel   : t_axilite_read_address_channel;
    read_data_channel      : t_axilite_read_data_channel;
  end record;

  subtype ST_AXILite_32 is t_axilite_if  (
    write_address_channel (
        awaddr(31 downto 0)  ),
    write_data_channel (
        wdata(31 downto 0),
        wstrb(3 downto 0)  ),
    read_address_channel (
        araddr(31 downto 0)  ),
    read_data_channel (
        rdata(31 downto 0)  )
    );

  signal s : st_axilite_32;
begin
  s.write_address_channel.awaddr <= x"0000_1000", x"1000_ffff" after 2 ns;
end;
