library ieee;
use ieee.std_logic_1164.all;

entity record_bug is
  port (
    clk_i       : in  std_logic;
    rst_i       : in  std_logic;
    test_o      : out std_logic_vector(7 downto 0)
    );
end record_bug;

architecture arch of record_bug is
  constant ZERO       : std_logic_vector(7 downto 0) := (others => '0');
  constant ELEMENTS   : integer := 2; -- bug happens for array elements number > 1 && < 6

  type array_type is array (0 to ELEMENTS-1) of std_logic_vector(7 downto 0);

  type record_type is record
    x   : std_logic_vector(7 downto 0);
    a   : array_type;
  end record;

  type record_array_type is array (0 to ELEMENTS-1) of record_type;   

  type state_type is (STATE0, STATE1, STATE2);

  signal state    : state_type;
  signal i        : integer range 0 to ELEMENTS-1;
  signal j        : integer range 0 to ELEMENTS-1;
  signal v        : record_array_type;
begin
  test_o <= v(0).x;   -- should be AA after reset deassertion, but in synth mode BB is assigned in STATE1

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
              v(0).x  <= X"AA";   -- assignment of other elements here does not affect buggy behavior
              state   <= STATE1;
                    
            when STATE1 =>
              v(i).a(j) <= X"BB"; -- v(0).x should not be touched here
                    
              if i /= ELEMENTS-1 then
                i <= i + 1;
              else
                if j /= ELEMENTS-1 then
                  j <= j + 1;
                  i <= 0;
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
