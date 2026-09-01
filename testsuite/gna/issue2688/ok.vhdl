library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--  The same port map as inst2, with the slices written ascending.  LRM08
--  5.3.2.2 e) 2) gives a formal that is constrained by its association --
--  by one slice or by several -- the direction of the index subtype of its
--  base type, 'natural' here, hence 'to'.  So this is the shape that is
--  legal, for the single slice of r_data as well as for the two of c_data.
entity ok is
    port (
        aclk    : in  std_logic;
        aresetn : in  std_logic;
        wvalid  : in  std_logic;
        wready  : out std_logic;
        wdata   : in  std_logic_vector(31 downto 0);
        wstrb   : in  std_logic_vector( 3 downto 0)
    );
end entity ok;

architecture struct of ok is
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
        r_data(0 to wdata'length+wstrb'high)  => wstrb & wdata,
        c_valid => bi_wvalid,
        c_ready => bi_wready,
        c_data(0 to wdata'high) => bi_wdata,
        c_data(wdata'length to wdata'length+wstrb'high) => bi_wstrb
    );

end architecture struct;
