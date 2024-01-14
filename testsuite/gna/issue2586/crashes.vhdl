entity crashes is
   generic (type data_type);
   port (i : in data_type);

   attribute altera_attribute      : string;
   -- comment out the following line for compilation to succeed
   attribute altera_attribute of i : signal is "-name SYNCHRONIZATION_REGISTER_CHAIN_LENGTH 1";
end crashes;

architecture syn of crashes is
begin
end syn;
