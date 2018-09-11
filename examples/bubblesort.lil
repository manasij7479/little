void bubblesort(array a) {
  int i, j, n
  i := 0
  n := sizeof(a)
  while ((i < n)) {
    j := (i + 1)
    while ((j < n)) {
      if ((a[i] > a[j])) {
        int t
        t := a[j]
        a[j] := a[i]
        a[i] := t
      }
      j := (j + 1)
    }
    i := (i + 1)
  }
}
int main() {
  array z[4]
  int i
  i := 0
  while ((i < 4)) {
    z[i] := (10 - i)
  }
  bubblesort(z)
  for (i : z) {
    print(i)
  }
  return 0
}
