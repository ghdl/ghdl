library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity inst is
    port (
        aclk    : in  std_logic;
        aresetn : in  std_logic;
        wvalid  : in  std_logic;
        wready  : out std_logic;
        wdata   : in  std_logic_vector(31 downto 0);
        wstrb   : in  std_logic_vector( 3 downto 0)
    );
end entity inst;

architecture struct of inst is
    constant data_bytes : natural := 4;
    signal bi_wvalid, bi_wready   : std_logic;
    signal bi_wdata : std_logic_vector(data_bytes*8-1 downto 0);
    signal bi_wstrb : std_logic_vector(data_bytes-1   downto 0);
begin

    w_skid : entity work.skid_buffer_in
    port map(
        aclk    => aclk,
        aresetn => aresetn,
        r_valid => wvalid,
        r_ready => wready,
        r_data  => wstrb & wdata,
        c_valid => bi_wvalid,
        c_ready => bi_wready,
        c_data(wdata'length+wstrb'high downto wdata'length) => bi_wstrb,
        c_data(wdata'high downto 0) => bi_wdata
    );

end architecture struct;
