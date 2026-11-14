--  Errors: each top must fail at elaboration.
entity err_leaf is
end err_leaf;

architecture rtl of err_leaf is
  signal v : bit_vector (3 downto 0) := "0000";
  constant c : integer := 1;
begin
end rtl;

entity e1 is
end e1;

architecture tb of e1 is
begin
  u : entity work.err_leaf;
  process
    alias v is << signal .wrong.u.v : bit_vector (3 downto 0) >>;
  begin
    wait;
  end process;
end tb;

entity e2 is
end e2;

architecture tb of e2 is
begin
  u : entity work.err_leaf;
  process
    alias v is << signal .e2.u.v : integer >>;
  begin
    wait;
  end process;
end tb;

entity e3 is
end e3;

architecture tb of e3 is
begin
  u : entity work.err_leaf;
  process
    alias v is << signal .e3.u.v : bit_vector (7 downto 0) >>;
  begin
    wait;
  end process;
end tb;

entity e4 is
end e4;

architecture tb of e4 is
begin
  u : entity work.err_leaf;
  process
    alias v is << signal .e4.u.nope : bit >>;
  begin
    wait;
  end process;
end tb;

entity e5 is
end e5;

architecture tb of e5 is
begin
  u : entity work.err_leaf;
  process
    alias v is << signal .e5.u.c : integer >>;
  begin
    wait;
  end process;
end tb;

entity e6 is
end e6;

architecture tb of e6 is
begin
  g : for i in 0 to 1 generate
    u : entity work.err_leaf;
  end generate;
  process
    alias v is << signal .e6.g(5).u.v : bit_vector (3 downto 0) >>;
  begin
    wait;
  end process;
end tb;

--  The external name is evaluated when the declaration is elaborated, before
--  the instance.
entity e7 is
end e7;

architecture tb of e7 is
  alias v is << signal .e7.u.v : bit_vector (3 downto 0) >>;
begin
  u : entity work.err_leaf;
end tb;
