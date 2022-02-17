library IEEE;
context IEEE.IEEE_std_context;

entity TriangularCounter is
  generic (
    g_Precision : natural := 11
  );
  port (
    CLK     : in  std_logic;
    RST     : in  std_logic;
    EN      : in  std_logic;
    REF     : out unsigned(g_Precision-1 downto 0);
    TRIGGER : out std_logic
  );
end entity;

architecture arch of TriangularCounter is

  signal dir    : std_logic;
  signal cnt    : unsigned(REF'range);
  signal tg_max : std_logic;
  signal tg_min : std_logic;

begin

    process(RST, CLK)
    begin
      if RST then
        cnt <= (others=>'0');
        dir <= '0';
      elsif rising_edge(CLK) then
        if EN then
          cnt <= cnt-1 when dir else cnt+1;
          if tg_min or tg_max then
            dir <= not dir;
          end if;
        end if;
      end if;
    end process;

    tg_max <= (not dir) and (cnt ?= to_unsigned(2**g_Precision-2, REF));
    tg_min <= dir and (cnt ?= 1);

    REF <= cnt;
    TRIGGER <= tg_min;

end architecture;

