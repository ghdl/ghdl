library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity record_bug3 is
    port (
        clk_i       : in  std_logic;
        rst_i       : in  std_logic;
        x_o         : out std_logic_vector(7 downto 0);
        a_o         : out std_logic_vector(7 downto 0);
        i_o         : out std_logic_vector(7 downto 0);
        j_o         : out std_logic_vector(7 downto 0)
    );
end record_bug3;

architecture arch of record_bug3 is

constant ZERO       : std_logic_vector(7 downto 0) := (others => '0');
constant ELEMENTS   : integer := 3; 
constant ELEMENTS2  : integer := 5; 

type array_type is array (0 to ELEMENTS2-1) of std_logic_vector(7 downto 0);

type record_type is record
    x   : std_logic_vector(7 downto 0);
    a   : array_type;
end record;

type record_array_type is array (0 to ELEMENTS-1) of record_type;   

type state_type is (STATE0, STATE1, STATE2);

signal state    : state_type;
signal i        : integer range 0 to ELEMENTS-1;
signal j        : integer range 0 to ELEMENTS2-1;
signal v        : record_array_type;

begin

x_o     <= v(i).x;      -- should be i
a_o     <= v(i).a(j);   -- should be (i+1)*(j+1)
i_o     <= std_logic_vector(to_unsigned(i,8));
j_o     <= std_logic_vector(to_unsigned(j,8));


process(clk_i)
begin
    if Rising_edge(clk_i) then
        if rst_i = '1' then
            state   <= STATE0;
            v       <= (others => (ZERO, (others => ZERO)));
            i       <= 0;
            j       <= 0;
        else
            case state is
                when STATE0 =>
                    -- set all x values at the same time
                    for k in 0 to ELEMENTS-1 loop
                        v(k).x  <= std_logic_vector(to_unsigned(k,8)); 
                    end loop;
                    state   <= STATE1;
                    
                when STATE1 =>
                    -- cycle to fill all a values with (i+1)*(j+1)
                    v(i).a(j) <= std_logic_vector(to_unsigned((i+1)*(j+1),8));
                    
                    if i /= ELEMENTS-1 then
                        i <= i + 1;
                    else
                        if j /= ELEMENTS2-1 then
                            j <= j + 1;
                            i <= 0;
                        else
                            state <= STATE2;
                            j <= 0;
                        end if;
                    end if;
                    
                when STATE2 =>
                    -- cycle infinitely for verification
                    if i /= ELEMENTS-1 then
                        i <= i + 1;
                    else
                        if j /= ELEMENTS2-1 then
                            j <= j + 1;
                            i <= 0;
                        else
                            j <= 0;
                        end if;
                    end if;
            end case;
        end if;
    end if;
end process;

end arch;
