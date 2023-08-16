library ieee;
use ieee.std_logic_1164.all;

entity general_fifo is
    generic (
        DATA_WIDTH:     positive;
        LOG2_DEPTH:     natural
    );
    port (
        reset:          in  std_logic;
        clock:          in  std_logic;
        push:           in  std_logic;
        din:            in  std_logic_vector (DATA_WIDTH - 1 downto 0);
        full:           out std_logic;
        empty:          out std_logic;
        dout:           out std_logic_vector (DATA_WIDTH - 1 downto 0);
        pull:           in  std_logic
    );
end entity;

architecture foo of general_fifo is
begin
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity bug_from_2417_fix is
end entity;

architecture good_faith of bug_from_2417_fix is
    
    constant ID_WIDTH:  natural := 4;
    type wbyp_type  is record
        id:     std_logic_vector (ID_WIDTH - 1 downto 0);
        resp:   std_logic_vector (ID_WIDTH + 1 downto ID_WIDTH);
        last:   std_logic;
    end record;
    constant DATA_WIDTH:    natural := ID_WIDTH + 3;
    
    constant  BYP_FIFO_DEPTH_LOG2:  natural := 16;

    signal reset:       std_logic;
    signal aclk:        std_logic;
    signal push:        std_logic;
    signal din:         std_logic_vector (DATA_WIDTH - 1 downto 0);
    signal full:        std_logic;
    signal empty:       std_logic;
    signal dout:        std_logic_vector (DATA_WIDTH - 1 downto 0);
    signal pull:        std_logic;
    
    signal wbyp_in:     wbyp_type;
    signal wbyp_out:    wbyp_type;
    signal wbyp_push:   std_logic;
    signal wbyp_full:   std_logic;
    signal wbyp_empty:  std_logic;
    signal wbyp_pull:   std_logic;
    
    component general_fifo is
        generic (
            DATA_WIDTH:     positive;
            LOG2_DEPTH:     natural
        );
        port (
            reset:          in  std_logic;
            clock:          in  std_logic;
            push:           in  std_logic;
            din:            in  std_logic_vector (DATA_WIDTH - 1 downto 0);
            full:           out std_logic;
            empty:          out std_logic;
            dout:           out std_logic_vector (DATA_WIDTH - 1 downto 0);
            pull:           in  std_logic
        );
    end component;

begin
    
    
wbypass: 
    if TRUE generate
        subtype ID_RANGE   is natural range ID_WIDTH-1 downto 0;
    	subtype RESP_RANGE is natural range ID_WIDTH+1 downto ID_WIDTH;
    	constant LAST_IDX : natural := ID_WIDTH+2;
    	signal data_in  : std_logic_vector(LAST_IDX downto 0);
    	signal data_out : std_logic_vector(LAST_IDX downto 0);
    begin
    	data_in(ID_RANGE)   <= wbyp_in.id;
    	data_in(RESP_RANGE) <= wbyp_in.resp;
    	data_in(LAST_IDX)   <= wbyp_in.last;

fifo:
        general_fifo
		    generic map (
    			DATA_WIDTH => data_in'length,
    			LOG2_DEPTH => BYP_FIFO_DEPTH_LOG2 -- this must be >= log2(LATENCY)
		    )
    		port map (
    			reset => reset,
    			clock => aclk,

    			push  => wbyp_push,
    			din   => data_in,
    			full  => wbyp_full,

    			empty => wbyp_empty,
    			dout  => data_out,
    			pull  => wbyp_pull
    		);
    	wbyp_out.id   <= data_out(ID_RANGE);
    	wbyp_out.resp <= data_out(RESP_RANGE);
    	wbyp_out.last <= data_out(LAST_IDX);
    end generate;
end architecture;
