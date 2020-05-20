library ieee ;

entity mytestbench is
end mytestbench;

architecture arch of mytestbench is
  signal zero_length_array : bit_vector(-1 downto 0);
begin

  -- Just here so we get a meaningful dump.
  main_process: process
  begin
    wait for 10 ns;
    wait;
  end process;      

end arch; 
