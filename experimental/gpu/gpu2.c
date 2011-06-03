// Simplified GPU interface
extern void setup_gpu();
extern void copy_to_gpu(int *x,int n);
extern void copy_from_gpu(int *x,int n);
extern void exec_gpu_kernel(char *id);
extern void teardown_gpu();

void foo(int *x, int n) {
  copy_to_gpu(x,n);
  exec_gpu_kernel("foo");
  copy_from_gpu(x,n);
}

void bar(int *x, int n) {
  copy_to_gpu(x,n);
  exec_gpu_kernel("bar");
  copy_from_gpu(x,n);
}

int main() {
  int n = 100;
  int *x = (int *)calloc(sizeof(int),n);
  setup_gpu();
  copy_to_gpu(x,n);
  exec_gpu_kernel("foo");
  copy_from_gpu(x,n);
  copy_to_gpu(x,n);
  exec_gpu_kernel("bar");
  copy_from_gpu(x,n);
  teardown_gpu();
  free(x);
  return 0;
}
