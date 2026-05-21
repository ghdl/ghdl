entity repro3 is
end entity;

architecture test of repro3 is
 type bvv_t   is array(natural range <>) of bit_vector;
 subtype ram_t is bvv_t(0 to 1)(1 downto 0);

 signal ram : ram_t;
 
 -- alias element is ram_t'element;    -- error because ram_t is not an object
 alias element_t is ram'element;

 signal ram2 : element_t;
begin

end architecture; 
