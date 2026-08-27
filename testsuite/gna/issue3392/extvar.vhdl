-- External name denoting a shared variable of a protected type (LRM 08 8.7).
package p is
  type ctr_t is protected
    procedure set (v : integer);
    impure function get return integer;
  end protected ctr_t;
end package p;

package body p is
  type ctr_t is protected body
    variable x : integer := 42;
    procedure set (v : integer) is begin x := v; end procedure;
    impure function get return integer is begin return x; end function;
  end protected body ctr_t;
end package body p;

entity sub is end entity sub;
architecture a of sub is
  shared variable c : work.p.ctr_t;
begin
end architecture a;

entity extvar is end entity extvar;
architecture a of extvar is
begin
  h0 : entity work.sub;
  process
    variable v : integer;
  begin
    v := << variable h0.c : work.p.ctr_t >>.get;   -- crashes here
    report "got " & integer'image(v);
    wait;
  end process;
end architecture a;
