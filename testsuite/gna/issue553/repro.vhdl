
entity Stimulus_Response is
generic ( CLK_PERIOD : Time := 20 ns );
port (
    NRESET          : in bit;
    CLK             : in bit;

    A   : out bit
);
end Stimulus_Response;

architecture Behavioral of Stimulus_Response is
    type my_bool is (True, False, Maybe);

    constant Scrubbing_Test     : False;
    constant MEM_Test           : boolean := False; 

    signal A_int : bit;
 
begin

    A   <= A_int;

end Behavioral;
