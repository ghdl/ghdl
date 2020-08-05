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
end tb;
