library ieee;
use ieee.std_logic_1164.all;

-- COMPONENT
entity a is
   -- N_BITS_DATA is nowhere initialized. This problem should be catched during
   -- elaboration but it isn't !
   -- During simulation I found that the value of N_BITS_DATA is -2147483648 and
   -- that the value of N_BITS_DATA-1 is 2147483647 !!!!
   generic (N_BITS_DATA : integer);
end entity;

architecture arch_a of a is
   --~ -- Here data_s will have 4_194_305 elements and this will make ghdl
   --~ -- take about 650 MB of memory. According to that, each element take about 150 B
   --~ signal data_s : std_logic_vector((N_BITS_DATA-1)/512 downto 0);

   -- This line make ghdl eat all the free memory because it is trying to make a
   -- vector of 2**31 elements !!!! And there isn't enough memory because we need
   -- about 2**31 * 150 B = ~ 300 GB !!!!
   signal data_s : std_logic_vector(N_BITS_DATA-1 downto 0);

   --~ -- Strangely this line doesn't make the simulation failed because N_BITS_DATA
   --~ -- is negativ, but it doesn't increase the use of memory either.
   --~ signal data_s : std_logic_vector(N_BITS_DATA downto 0);
begin
   process begin
      -- N_BITS_DATA = -2147483648 = -2**31
      report integer'image(N_BITS_DATA);
      --
      -- N_BITS_DATA-1 = 2147483647 = 2**31 - 1
      report integer'image(N_BITS_DATA-1);
      --
      -- (N_BITS_DATA-1)/512 = 4_194_304 = 2**22
      report integer'image((N_BITS_DATA-1)/512);
      --
   end process;
end;
--

-- TESTBENCH
entity tb is end entity;

architecture arch_tb of tb is
begin
   X1: entity work.a;
end;
--
