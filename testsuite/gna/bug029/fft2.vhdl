package fft_package is
  TYPE complex IS ARRAY(0 TO 1) OF INTEGER;
  function butterfly(X1, X2 , W : complex )return complex;
END fft_package;

package body fft_package is

    function butterfly ( X1, X2 , W : complex )return complex is

    BEGIN
         return X1;
         return X2;
    END butterfly;
end fft_package;
