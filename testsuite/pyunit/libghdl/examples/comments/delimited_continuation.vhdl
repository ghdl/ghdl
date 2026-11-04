entity e is
  port (
    a : in bit;  /* first */
                 /* second */
                 -- third
    b : in bit;  /* first,
                 on two lines */
                 /* second */
    c : in bit;  /* first */

                 /* another column */
    d : in bit;  /* first */
      /* empty line after */

    e : in bit;
    f : in bit;
                 /* not a continuation */
    g /* inside */ : in bit;  /* after the token */
                              /* and its continuation */
    h : in bit; /* first */ /* same line */ -- also the same line
    i : in bit; /* ends on the line
    */ /* of the next one */
    z : in bit
  );
end entity;
