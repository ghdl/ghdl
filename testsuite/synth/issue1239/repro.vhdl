library ieee;
use ieee.std_logic_1164.all;
use IEEE.std_logic_arith.all;
-------------------------------------------------------------------------------------
--
--
-- Definition of Ports
-- ACLK           : Synchronous clock
-- ARESETN        : System reset, active low
-- S_AXIS_TREADY  : Ready to accept data in
-- S_AXIS_TDATA   : Data in 
-- S_AXIS_TLAST   : Optional data in qualifier
-- S_AXIS_TVALID  : Data in is valid
-- M_AXIS_TVALID  : Data out is valid
-- M_AXIS_TDATA   : Data Out
-- M_AXIS_TLAST   : Optional data out qualifier
-- M_AXIS_TREADY  : Connected slave device is ready to accept data out
--
-------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Entity Section 
------------------------------------------------------------------------------
entity accelerator is
GENERIC( 
  	constant DATA_WIDTH 		: positive := 8;
	constant IMAGE_WIDTH 		: positive := 13;
	constant IMAGE_SIZE 		: positive := 169;
	constant DOUT_WIDTH		: positive := 5 -- TO BE CALCULATED
		); 

	port(
		-- DO NOT EDIT BELOW THIS LINE ---------------------
		-- Bus protocol ports, do not add or delete. 
		ACLK            : in	std_logic;
		ARESETN         : in	std_logic;
		S_AXIS_TREADY	: out	std_logic;
		S_AXIS_TDATA	: in	std_logic_vector(31 downto 0);
		S_AXIS_TLAST	: in	std_logic;
		S_AXIS_TVALID	: in	std_logic;
		M_AXIS_TVALID	: out	std_logic;
		M_AXIS_TDATA	: out	std_logic_vector(15 downto 0);
		--M_AXIS_TLAST	: out	std_logic;
		M_AXIS_TREADY	: in	std_logic;
		EN_LOC_STREAM_1: in std_logic
		-- DO NOT EDIT ABOVE THIS LINE ---------------------
	);

end accelerator;

------------------------------------------------------------------------------
-- Architecture Section
------------------------------------------------------------------------------
architecture Behavior of accelerator is
signal DOUT_1_1          : std_logic_vector(DOUT_WIDTH-1 downto 0);
signal DOUT_2_1          : std_logic_vector(DOUT_WIDTH-1 downto 0);
signal EN_STREAM_OUT_1	 : std_logic;
signal VALID_OUT_1       : std_logic;
signal INTERNAL_RST : std_logic;
---------------------------------- MAP NEXT LAYER - COMPONENTS START----------------------------------
COMPONENT CONV_LAYER_1
   port(
	DIN                 :IN std_logic_vector(DATA_WIDTH-1 downto 0);
	CLK,RST             :IN std_logic;
	EN_STREAM           :IN std_logic; 				-- S_AXIS_TREADY  : Ready to accept data in 
	EN_STREAM_OUT_1     :OUT std_logic; 				-- M_AXIS_TREADY  : Connected slave device is ready to accept data out/ Internal Enable
	VALID_OUT_1         :OUT std_logic;  				-- M_AXIS_TVALID  : Data out is valid
	EN_LOC_STREAM_1     :IN std_logic;
	DOUT_1_1            :OUT std_logic_vector(DOUT_WIDTH-1 downto 0);
	DOUT_2_1            :OUT std_logic_vector(DOUT_WIDTH-1 downto 0);
	INTERNAL_RST        :OUT std_logic
      			);
END COMPONENT CONV_LAYER_1;

begin

CONV_LYR_1 : CONV_LAYER_1 
          port map(
           CLK               => ACLK,
           RST               => ARESETN,
           DIN               => S_AXIS_TDATA(7 downto 0),
           EN_STREAM         => M_AXIS_TREADY,		
           DOUT_1_1               => DOUT_1_1,
           DOUT_2_1               => DOUT_2_1,
           EN_LOC_STREAM_1 => EN_LOC_STREAM_1,
           EN_STREAM_OUT_1        => EN_STREAM_OUT_1,
		   VALID_OUT_1            => VALID_OUT_1,
		   INTERNAL_RST => INTERNAL_RST
          );

 M_AXIS_TDATA(4 downto 0) <= DOUT_1_1;
 M_AXIS_TDATA(9 downto 5)<= DOUT_2_1;
 M_AXIS_TDATA (15 downto 10) <= (others => '0');
 S_AXIS_TREADY<= EN_STREAM_OUT_1;
 M_AXIS_TVALID<= VALID_OUT_1;
 
 
end architecture Behavior;
