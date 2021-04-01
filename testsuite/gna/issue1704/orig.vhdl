subtype word is std_logic_vector(word_len - 1 downto 0);
function F return word is 
begin
  return ("1010","101010101010", others => '0');
end function;   
