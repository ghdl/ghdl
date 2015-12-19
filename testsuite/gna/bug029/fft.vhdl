package fft_package is
  TYPE complex IS ARRAY(0 TO 1) OF INTEGER;
  CONSTANT w0 : complex := (1, 0);  --Pre-computed constants
  CONSTANT w1 : complex := (0, -1); --Pre-computed constants

  function butterfly(X1, X2 , W : complex )return complex;
END fft_package;

package body fft_package is

    function butterfly ( X1, X2 , W : complex )return complex is
      VARIABLE Y1, Y2  : complex;

    BEGIN
        -- G1 = X1 + W*X2
        G1:Y1(0) := X1(0) + ((W(0)*X2(0)) - W(1)*X2(1)); -- G1 real
        Y1(1) := X1(1) + ((W(0)*X2(1)) + W(1)*X2(0)); -- G1 imaginary

        -- G2 = X1 - W*X2
        Y2(0) := X1(0) - ((W(0)*X2(0)) - W(1)*X2(1)); -- G2 real
        Y2(1) := X1(1) - ((W(0)*X2(1)) + W(1)*X2(0)); -- G2 imaginary
         return Y1;
         return Y2;
    END butterfly;
end fft_package;
