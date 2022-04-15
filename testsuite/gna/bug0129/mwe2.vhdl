entity MWE is
  port (
    clk      : in bit;
    start    : in bit;
    continue : in bit;
    tready   : in bit
    );
end;

architecture RTL of MWE is

  constant C_CTR_MAX : natural := 10;

  type t_state is (e_idle, e_count, e_do_something_else);

  type t_rec is record
    state : t_state;
    ctr   : natural range 0 to C_CTR_MAX + 5;
  end record t_rec;

  signal r      : t_rec := (e_idle, 0);
  signal r_next : t_rec;

begin

  p_main: process (all)

    procedure count (
      constant r_ctr : in  natural;
      variable v_ctr : out natural
    ) is
    begin
      if r_ctr < C_CTR_MAX then
        if tready then
          v_ctr := r_ctr + 1;
        else
          v_ctr := r_ctr;
        end if;
--    else              <-- missing
--      v_ctr := r_ctr;
      end if;
    end procedure;

    variable v : t_rec;

  begin

    v := r;

    case r.state is

      when e_idle =>
        if start then
          v.state := e_count;
        end if;

      when e_count =>
        count(r.ctr, v.ctr);

        if continue then
          v.ctr   := 0;
          v.state := e_do_something_else;
        end if;

      when e_do_something_else =>
        v.state := e_idle;

    end case;

    r_next <= v;

  end process p_main;

  p_update_state : process (clk)
  begin
    if rising_edge(clk) then
      r <= r_next;
    end if;
  end process;

end;
use std.env.stop;

entity MWE_TB is
end;

architecture Sim of MWE_TB is

  signal clk      : bit := '0';
  signal start    : bit;
  signal continue : bit;
  signal tready   : bit;

begin

  clk <= not clk after 5 ns;

  i_DUT : entity work.MWE(RTL)
    port map (
      clk      => clk,
      start    => start,
      continue => continue,
      tready   => tready
    );

  p_run : process

  begin

    start    <= '0';
    continue <= '0';
    tready   <= '0';

    wait for 15 ns;

    start <= '1';

    wait for 10 ns;

    start <= '0';

    wait for 10 ns;

    tready <= '1';

    wait for 150 ns;

    continue <= '1';

    wait for 10 ns;

    continue <= '0';

    wait for 50 ns;

    std.env.stop;

  end process;

end;
