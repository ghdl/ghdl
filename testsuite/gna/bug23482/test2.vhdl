entity test2 is end entity; 

architecture arch of test2 is 
  signal b:bit; 
  -- alias bit_base is bit'base; 
  alias b_stable is b'stable; 
begin 
end architecture; 
