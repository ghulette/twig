float *twig_gen_fun_opt(float *in) {
  float *tmp01,*tmp02,*tmp03,*tmp04,*tmp05;
  tmp01 = in;
  cudaMalloc((void **)&tmp02,SIZE);
  cudaMemcpy(tmp02,tmp01,SIZE,cudaMemcpyHostToDevice);
  foo <<<N_BLOCKS,BLOCK_SIZE>>> (tmp02,N);
  tmp03 = tmp02;
  bar <<<N_BLOCKS,BLOCK_SIZE>>> (tmp03,N);
  tmp04 = tmp03;
  tmp05 = (float *)malloc(SIZE);
  cudaMemcpy(tmp05,tmp04,SIZE,cudaMemcpyDeviceToHost);
  return tmp05;
}

float *twig_gen_fun_orig(float *in) {
  float *tmp01,*tmp02,*tmp03,*tmp04,*tmp05,*tmp06,*tmp07;
  tmp01 = in;
  cudaMalloc((void **)&tmp02,SIZE);
  cudaMemcpy(tmp02,tmp01,SIZE,cudaMemcpyHostToDevice);
  foo <<<N_BLOCKS,BLOCK_SIZE>>> (tmp02,N);
  tmp03 = tmp02;
  tmp04 = (float *)malloc(SIZE);
  cudaMemcpy(tmp04,tmp03,SIZE,cudaMemcpyDeviceToHost);
  cudaMalloc((void **)&tmp05,SIZE);
  cudaMemcpy(tmp05,tmp04,SIZE,cudaMemcpyHostToDevice);
  bar <<<N_BLOCKS,BLOCK_SIZE>>> (tmp05,N);
  tmp06 = tmp05;
  tmp07 = (float *)malloc(SIZE);
  cudaMemcpy(tmp07,tmp06,SIZE,cudaMemcpyDeviceToHost);
  return tmp07;
}