library ieee;
use ieee.std_logic_1164.all;

entity inst3 is
    port (
        aclk    : in  std_logic;
        aresetn : in  std_logic;
        wvalid  : in  std_logic;
        wready  : out std_logic
    );
end entity inst3;

architecture struct of inst3 is
    constant data_bytes : natural := 4;
    signal bi_wvalid, bi_wready : std_logic;
    signal bi_wblob : std_logic_vector(data_bytes*9-1 downto 0);
    constant left  : std_logic_vector(data_bytes-1   downto 0) := (others => '1');
    constant right : std_logic_vector(data_bytes*8-1 downto 0) := (others => '1');
begin
    w_skid : entity work.skid_buffer_in
    port map(
        aclk    => aclk,
        aresetn => aresetn,
        r_valid => wvalid,
        r_ready => wready,
        r_data  => left & right,
        c_valid => bi_wvalid,
        c_ready => bi_wready,
        c_data  => bi_wblob
    );
end architecture struct;
