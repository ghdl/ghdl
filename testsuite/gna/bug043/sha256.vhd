library ieee;
use     ieee.std_logic_1164.all;
use     ieee.numeric_std.all;
--use IEEE.std_logic_unsigned.all;

ENTITY sha256forBTC is
    Port ( reset 	: in  STD_LOGIC;
           clock 	: in  STD_LOGIC;
           --data input signals
           data 	: in  STD_LOGIC_VECTOR (511 downto 0);
           enable : in  STD_LOGIC;
           busy   : out STD_LOGIC;
           --hash output signals
           digest	: out STD_LOGIC_VECTOR (255 downto 0);
           ready 	: out STD_LOGIC);
end sha256forBTC;

ARCHITECTURE Behavioral of sha256forBTC is

   type hT is array (0 to 7) of STD_LOGIC_VECTOR(31 downto 0);
   constant hInit : hT :=
     (x"6a09e667", x"bb67ae85", x"3c6ef372", x"a54ff53a", x"510e527f", x"9b05688c", x"1f83d9ab", x"5be0cd19");
   type kT is array (0 to 63) of STD_LOGIC_VECTOR(31 downto 0);
   constant k : kT :=
     (x"428a2f98", x"71374491", x"b5c0fbcf", x"e9b5dba5", x"3956c25b", x"59f111f1", x"923f82a4", x"ab1c5ed5",
      x"d807aa98", x"12835b01", x"243185be", x"550c7dc3", x"72be5d74", x"80deb1fe", x"9bdc06a7", x"c19bf174",
      x"e49b69c1", x"efbe4786", x"0fc19dc6", x"240ca1cc", x"2de92c6f", x"4a7484aa", x"5cb0a9dc", x"76f988da",
      x"983e5152", x"a831c66d", x"b00327c8", x"bf597fc7", x"c6e00bf3", x"d5a79147", x"06ca6351", x"14292967",
      x"27b70a85", x"2e1b2138", x"4d2c6dfc", x"53380d13", x"650a7354", x"766a0abb", x"81c2c92e", x"92722c85",
      x"a2bfe8a1", x"a81a664b", x"c24b8b70", x"c76c51a3", x"d192e819", x"d6990624", x"f40e3585", x"106aa070",
      x"19a4c116", x"1e376c08", x"2748774c", x"34b0bcb5", x"391c0cb3", x"4ed8aa4a", x"5b9cca4f", x"682e6ff3",
      x"748f82ee", x"78a5636f", x"84c87814", x"8cc70208", x"90befffa", x"a4506ceb", x"bef9a3f7", x"c67178f2");
   signal a,b,c,d,e,f,g,h,h0,h1,h2,h3,h4,h5,h6,h7,s0,s1,su0,su1,maj,ch,temp1,temp2 : STD_LOGIC_VECTOR(31 downto 0);
   type wT is array (15 downto 0) of STD_LOGIC_VECTOR(31 downto 0);
   signal w : wT;
   signal wCNT, chunkCNT : STD_LOGIC_VECTOR(6 downto 0);
   signal intEnable : STD_LOGIC;

BEGIN

   fsm: process(clock)
   begin
      if rising_edge(clock) then
         if reset = '1' then
            chunkCNT <= "1000000";
         else
            if chunkCNT = "1000000" then
               if enable = '1' then
                  chunkCNT <= "0000000";
               end if;
            else
               chunkCNT <= std_logic_vector(unsigned(chunkCNT) + 1);
            end if;
         end if;
      ready <= not intEnable;
      end if;
   end process;

   intEnable <= not chunkCNT(6);
   busy <= intEnable;
   
   extension_pipe: process(clock)
   begin
      if rising_edge(clock) then
         if enable = '1' then
            w(0) <= data(31 downto 0);
            w(1) <= data(63 downto 32);
            w(2) <= data(95 downto 64);
            w(3) <= data(127 downto 96);
            w(4) <= data(159 downto 128);
            w(5) <= data(191 downto 160);
            w(6) <= data(223 downto 192);
            w(7) <= data(255 downto 224);
            w(8) <= data(287 downto 256);
            w(9) <= data(319 downto 288);
            w(10) <= data(351 downto 320);
            w(11) <= data(383 downto 352);
            w(12) <= data(415 downto 384);
            w(13) <= data(447 downto 416);
            w(14) <= data(479 downto 448);
            w(15) <= data(511 downto 480);
         elsif intEnable = '1' then
	 w <= w(14 downto 0) & std_logic_vector(unsigned(w(15)) + unsigned(s0) + unsigned(w(6)) + unsigned(s1));
         end if;
      end if;
   end process;
   --extension_pipe asynchron circuitry
   s0 <= (w(14)(6 downto 0) & w(14)(31 downto 7)) xor (w(14)(17 downto 0) & w(14)(31 downto 18)) xor ("000" & w(14)(31 downto 3));
   s1 <= (w(1)(16 downto 0) & w(1)(31 downto 17)) xor (w(1)(18 downto 0) & w(1)(31 downto 19)) xor ("0000000000" & w(1)(31 downto 10));
   --end of extension_pipe asynchron circuitry

   main_loop_pipe: process(clock)
   begin
      if rising_edge(clock) then
         if reset = '1' then
            a <= hInit(0);
            b <= hInit(1);
            c <= hInit(2);
            d <= hInit(3);
            e <= hInit(4);
            f <= hInit(5);
            g <= hInit(6);
            h <= hInit(7);
         elsif intEnable = '0' then
            a <= std_logic_vector(unsigned(h0) + unsigned(a));
            b <= std_logic_vector(unsigned(h1) + unsigned(b));
            c <= std_logic_vector(unsigned(h2) + unsigned(c));
            d <= std_logic_vector(unsigned(h3) + unsigned(d));
            e <= std_logic_vector(unsigned(h4) + unsigned(e));
            f <= std_logic_vector(unsigned(h5) + unsigned(f));
            g <= std_logic_vector(unsigned(h6) + unsigned(g));
            h <= std_logic_vector(unsigned(h7) + unsigned(h));
         else
            h <= g;
            g <= f;
            f <= e;
            e <= std_logic_vector(unsigned(d) + unsigned(temp1));
            d <= c;
            c <= b;
            b <= a;
            a <= temp2;
         end if;
      end if;
   end process;
   --main_loop_pipe asynchron circuitry
   su1   <= (e(5 downto 0) & e(31 downto 6)) xor (e(10 downto 0) & e(31 downto 11)) xor (e(24 downto 0) & e(31 downto 25));
   ch    <= (e and f) xor ((not e) and g);
   temp1 <= std_logic_vector(unsigned(h) + 
	    unsigned(su1) + 
	    unsigned(ch) + 
	    unsigned(k(to_integer(unsigned(chunkCNT(5 downto 0))))) + 
	    unsigned(w(15)));
   su0   <= (a(1 downto 0) & a(31 downto 2)) xor (a(12 downto 0) & a(31 downto 13)) xor (a(21 downto 0) & a(31 downto 22));
   maj   <= (a and (b xor c)) xor (b and c);
   temp2 <= std_logic_vector(unsigned(temp1) + unsigned(su0) + unsigned(maj));
   --end of main_loop_pipe asynchron circuitry
   
   add_hash_chunk: process(clock)
   begin
      if rising_edge(clock) then
         if reset = '1' then
            h0 <= x"00000000";
            h1 <= x"00000000";
            h2 <= x"00000000";
            h3 <= x"00000000";
            h4 <= x"00000000";
            h5 <= x"00000000";
            h6 <= x"00000000";
            h7 <= x"00000000";
         else
            if intEnable = '0' then
               h0 <= std_logic_vector(unsigned(h0) + unsigned(a));
               h1 <= std_logic_vector(unsigned(h1) + unsigned(b));
               h2 <= std_logic_vector(unsigned(h2) + unsigned(c));
               h3 <= std_logic_vector(unsigned(h3) + unsigned(d));
               h4 <= std_logic_vector(unsigned(h4) + unsigned(e));
               h5 <= std_logic_vector(unsigned(h5) + unsigned(f));
               h6 <= std_logic_vector(unsigned(h6) + unsigned(g));
               h7 <= std_logic_vector(unsigned(h7) + unsigned(h));
            end if;
         end if;
      end if;
   end process;

   digest <= h0 & h1 & h2 & h3 & h4 & h5 & h6 & h7;

end Behavioral;
