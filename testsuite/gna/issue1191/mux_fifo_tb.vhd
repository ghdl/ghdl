library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mux_fifo_pkg.all;

entity mux_fifo_tb is
  generic (g_nb_channels : natural := 8;
           g_data_width  : natural := 16);
  port (rst              : in  std_logic;
        clk              : in  std_logic;
        -- fifo in if
        fifo_in_data     : in  std_logic_vector(g_data_width-1 downto 0);
        fifo_in_wr       : in  std_logic_vector(g_nb_channels-1 downto 0);
        fifo_in_full     : out std_logic_vector(g_nb_channels-1 downto 0);
        -- out if
        fifo_out_dataout : out std_logic_vector(g_data_width+8-1 downto 0);
        fifo_out_rd_en   : in  std_logic;
        fifo_out_empty   : out std_logic);
end entity mux_fifo_tb;

architecture test_tb of mux_fifo_tb is

  constant ones : std_logic_vector(g_nb_channels-1 downto 0) := (others => '1');

  signal dataout     : std_logic_vector(g_data_width+8-1 downto 0) := (others => '0');
  signal wr_en, full : std_logic                                   := '0';

  signal fifo_if_in : t_mux_fifo_if(data(g_nb_channels-1 downto 0)(g_data_width-1 downto 0),
                                    empty(g_nb_channels-1 downto 0),
                                    rd(g_nb_channels-1 downto 0));

begin

  --u_fifos_in : for i in fifo_in_wr'range generate
  --  u_fifo_in : entity work.mux_fifo_fifo
  --    port map (wr_clk => clk,
  --              wr_rst => rst,
  --              din    => fifo_in_data,
  --              wr_en  => fifo_in_wr(i),
  --              full   => fifo_in_full(i),
  --              --
  --              rd_clk => fifo_if_in.clk,
  --              rd_rst => rst,
  --              rd_en  => fifo_if_in.rd(i),
  --              dout   => fifo_if_in.data(i),
  --              empty  => fifo_if_in.empty(i));
  --end generate u_fifos_in;

  u_dut : entity work.mux_fifo
    generic map (g_enabled_channels => ones,
                 g_extend           => 1)
    port map (rst        => rst,
              clk        => clk,
              -- fifo if
              fifo_if_in => fifo_if_in,
              -- out if
              dataout    => dataout,
              wr_en      => wr_en,
              full       => full);

  --u_fifo_out : entity work.mux_fifo_fifo_out
  --  port map (wr_clk => clk,
  --            wr_rst => rst,
  --            din    => dataout,
  --            wr_en  => wr_en,
  --            full   => full,
  --            --
  --            rd_clk => clk,
  --            rd_rst => rst,
  --            rd_en  => fifo_out_rd_en,
  --            dout   => fifo_out_dataout,
  --            empty  => fifo_out_empty);

end architecture test_tb;

configuration mux_fifo_tb_test_tb_cfg of mux_fifo_tb is
  for test_tb
  end for;
end mux_fifo_tb_test_tb_cfg;
