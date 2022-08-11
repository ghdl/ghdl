entity array_of_string_elements is
end entity;

architecture foo of array_of_string_elements is
    type strings is array (natural range <>) of string;
    constant strings_constants: strings(0 to 3) := 
                          ("one", "two", "three", "four");
    
begin
end architecture;
