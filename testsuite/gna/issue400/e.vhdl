entity e is end entity;
architecture a of e is
  type p is protected
    function f(a:bit) return boolean;
  end protected;
  type p is protected body
    function f(a:bit) return boolean is begin return a='1'; end function;
  end protected body;
  shared variable v :p;
  component bb1 is port( s :out bit     ); end component;
  component bb2 is port( s :in  boolean ); end component;
  signal s1 :boolean;
  signal s2 :bit;
begin
  i1: component bb1 port map( v.f(s) => s1 );
  i2: component bb2 port map( s => v.f(s2) );
end architecture;
