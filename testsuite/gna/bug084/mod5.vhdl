library ieee;
use ieee.std_logic_1164.all;

entity mod5 is
    generic (
        NBITS:  natural := 13 
    );
    port (
        clk:        in  std_logic;
        dividend:   in  std_logic_vector (NBITS - 1 downto 0);
        load:       in  std_logic;
        remzero:    out std_logic
    );
end entity;

architecture foo of mod5 is
    type remains is (r0, r1, r2, r3, r4); -- remainder values
    type remain_array is array (NBITS downto 0) of remains;
    signal remaindr:    remain_array := (others => r0);
    type branch is array (remains, bit) of remains;
-- Dave Tweeds state transition table:
    constant br_table:  branch := ( r0 => ('0' => r0, '1' => r1),
                                    r1 => ('0' => r2, '1' => r3),
                                    r2 => ('0' => r4, '1' => r0),
                                    r3 => ('0' => r1, '1' => r2),
                                    r4 => ('0' => r3, '1' => r4)
                                  );
begin

do_ig:
    process (dividend)
        variable tbit:      bit_vector(NBITS - 1 downto 0);
        variable remaind:   remain_array := (others => r0);
    begin
do_mod:
        for i in NBITS - 1 downto 0 loop
             tbit := to_bitvector(dividend);
             remaind(i) := br_table(remaind(i + 1),tbit(i));
         end loop;
        remaindr <= remaind;   -- all values for waveform display
    end process;
    
remainders:
    process (clk)
    begin
        if rising_edge(clk) then 
            if remaindr(0) = r0 then
                remzero <= '1';
            else
                remzero <= '0';
            end if;
        end if;
    end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mod5_tb is
end entity;

architecture foo of mod5_tb is
    constant NBITS:    integer range 0 to 13 := 8;
    signal clk:        std_logic := '0';
    signal dividend:   std_logic_vector (NBITS - 1 downto 0);
    signal load:       std_logic := '0';
    
    signal remzero:    std_logic;
    
    signal psample:    std_ulogic;
    signal sample:     std_ulogic;
    signal done:       boolean;
begin
DUT:
    entity work.mod5
        generic map  (NBITS)
        port map (
            clk => clk,
            dividend => dividend,
            load => load,
            remzero => remzero
        );
CLOCK:
    process
    begin
        wait for 5 ns;
        clk <= not clk;
        if done'delayed(30 ns) then
            wait;
        end if;
    end process;
STIMULI:
    process
    begin
        for i in 0 to 2 ** NBITS - 1 loop
            wait for 10 ns;
            dividend <= std_logic_vector(to_unsigned(i,NBITS));
            wait for 10 ns;
            load <= '1';
            wait for 10 ns;
            load <= '0';
        end loop;
        wait for 15 ns;
        done <= true;
        wait;
    end process;
    
SAMPLER:
    process (clk)
    begin
        if rising_edge(clk) then
            psample <= load;
            sample <= psample;
        end if;
    end process;
    
MONITOR:
    process (sample)
        variable i:     integer;
        variable rem5:  integer;
    begin
        if rising_edge (sample) then
            i := to_integer(unsigned(dividend));
            rem5 := i mod 5;
            if rem5 = 0 and remzero /= '1' then
                assert rem5 = 0 and remzero = '1'
                    report LF & HT &
                        "i = " & integer'image(i) &
                        " rem 5 expected " & integer'image(rem5) & 
                        " remzero = " & std_ulogic'image(remzero)
                    SEVERITY ERROR;
            end if;
        end if;
    end process;
    
end architecture;