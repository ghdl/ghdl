entity crash is end;

architecture behav of crash is
  constant data_width : natural := 8;
  type data_type is record 
    data: bit_vector(data_width-1 downto 0); 
    enable: bit; 
  end record data_type; 
  type port_type is array(0 to 15) of data_type; 
  signal s : port_type;
begin
  s(s'range).enable <= '0';
end behav;
   
