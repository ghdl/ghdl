library ieee;
    use ieee.std_logic_1164.all;

entity repro1 is
  generic
  (       NB_CHAN_G : positive := 2
   ;      W_DAT_G   : positive := 16
  );
end;

architecture tb of repro1  is
  type fifo_o_t is record
    rx_ack   : std_ulogic;
    tx_cti   : std_ulogic_vector( 1 downto 0 );
    tx_dat   : std_ulogic_vector;
  end record;

  type fifo_array_o_t is array ( natural range <> ) of fifo_o_t;

  signal arr_fifo_o :
    fifo_array_o_t(0 to NB_CHAN_G - 1) (tx_dat(W_DAT_G - 1 downto 0));
begin
  arr_fifo_o (0).tx_dat(2) <= '1' after 1 ns, '0' after 2 ns;

  arr_fifo_o (1).tx_dat(3) <= '1' after 200 ps;
end tb;
