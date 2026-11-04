/* The header of the file, :header:
   over two lines. */

/* doc of :e: */
entity e is  /* line of the header of :e: */
  port (
    /* before :a: */
    a : in bit;  /* line of :a: */
                 /* continuation of :a: */
    /* before :b:, over
       two lines */
    b : in bit;  /* line of :b:, over
                    two lines */
                 -- continuation of :b:
    /* before the token of :c: */ c : in bit;  /* line of :c: */  -- again on the line of :c:
    d : in bit; /* before the token of :d2: */ d2 : in bit;
    f /* inside :f: */ : in bit;
    g : in bit /* inside :g: */; h : in bit
  );
end entity;

/* doc of :a1: */ architecture a1 of e is
  /* doc of :a1: followed by an empty line */

  /* before the token of :s: */ signal s : bit;
  signal t /* inside :t: */ : bit;  /* line of :t: */

  /* over two lines,
     before the token of :u: */ signal u : bit;

  --  mixed with a line comment for :v:
  /* and a delimited comment for :v: */
  signal v : bit;

  signal w : bit;  -- line comment for :w:
                   /* and a continuation of :w: */
begin
end architecture;
