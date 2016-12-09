int main1 (int a)
{
  int b;
  int z;

  b = a;

  {
    int g;
    int c;

    c = b;
    g = c;
    z = g + c;
  }
  return z + 1;
}
