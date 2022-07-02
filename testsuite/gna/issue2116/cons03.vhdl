library ieee;use ieee.numeric_std.all;use ieee.std_logic_1164.all;entity generic_fifo_fwft_inst is
port(u:std_logic;e:integer:=0;a:std_logic_vector(0 downto 0);t:std_logic_vector(0 to 0);e0:out std_logic;l:std_logic;r:std_logic;d:std_logic);end;architecture t of generic_fifo_fwft_inst is type mystream_t is record
x:std_logic_vector(0 to 0);y:integer range 0 to 0;end record;signal m:t'S mystream_t;signal i:t;begin
t(((0)));f generic map(0);end architecture;