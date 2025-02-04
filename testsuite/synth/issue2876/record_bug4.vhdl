library ieee;
use ieee.std_logic_1164.all;

entity record_bug4 is
  generic (
    NEL1 : natural := 3;
    NEL2 : natural := 4);
    port (
        clk_i       : in  std_logic;
        rst_i       : in  std_logic;
        test_o      : out std_logic_vector(7 downto 0)
    );
end;

architecture arch of record_bug4 is

constant ZERO       : std_logic_vector(7 downto 0) := (others => '0');

type array_type is array (0 to NEL1-1) of std_logic_vector(7 downto 0);

type record_type is record
    x   : std_logic_vector(7 downto 0);
    a   : array_type;
end record;

type record_array_type is array (0 to NEL2 - 1) of record_type;   

type state_type is (STATE0, STATE1, STATE2);

signal state    : state_type;
signal i        : integer range record_array_type'range;
signal j        : integer range array_type'range;
signal v        : record_array_type;

begin

  test_o <= v(0).a(1);   -- should be AA after STATE1 deassertion

  process(clk_i)
  begin
    if Rising_edge(clk_i) then
      if rst_i = '1' then
        state   <= STATE0;
        v       <= (others => (ZERO, (others => ZERO)));
        i       <= record_array_type'low;
        j       <= array_type'low;
      else
        case state is
          when STATE0 =>
            v(0).x  <= X"BB";   
            state   <= STATE1;
                    
          when STATE1 =>
            v(i).a(j) <= X"AA";
                    
            if i /= record_array_type'high then
              i <= i + 1;
            else
              if j /= array_type'high then
                j <= j + 1;
                i <= record_array_type'low;
              else
                state <= STATE2;
              end if;
            end if;
                    
          when STATE2 =>
            null;
        end case;
      end if;
    end if;
  end process;
end arch;
