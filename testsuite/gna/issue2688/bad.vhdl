library ieee;
use ieee.std_logic_1164.all;

--  The formal is sliced in two parts that do not have the same direction,
--  so there is no direction to give to the unconstrained port: the slices
--  are reported, and GHDL used to crash right after the error.
entity bad is
    port (
        aclk    : in  std_logic;
        aresetn : in  std_logic;
        wvalid  : in  std_logic;
        wready  : out std_logic;
        wdata   : in  std_logic_vector(31 downto 0);
        wstrb   : in  std_logic_vector( 3 downto 0)
    );
end entity bad;

architecture struct of bad is
    signal bi_wvalid, bi_wready : std_logic;
    signal bi_wdata : std_logic_vector(31 downto 0);
    signal bi_wstrb : std_logic_vector( 3 downto 0);
begin

    w_skid : entity work.skid_buffer_in
    port map(
        aclk    => aclk,
        aresetn => aresetn,
        r_valid => wvalid,
        r_ready => wready,
        r_data(35 downto 0) => wstrb & wdata,
        c_valid => bi_wvalid,
        c_ready => bi_wready,
        c_data(35 downto 32) => bi_wstrb,
        c_data(0 to 31)      => bi_wdata
    );

end architecture struct;
