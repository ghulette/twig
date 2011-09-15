float *tmp01,*tmp02,*tmp03,*tmp04,*tmp05;

cudaMalloc((void **)&tmp02,SIZE);
cudaMemcpy(tmp02,tmp01,SIZE,cudaMemcpyHostToDevice);

foo <<<N_BLOCKS,BLOCK_SIZE>>> (tmp02, N);
tmp03 = tmp02;

bar <<<N_BLOCKS,BLOCK_SIZE>>> (tmp03,N);
tmp04 = tmp03;

tmp05 = malloc(SIZE * sizeof(float));
cudaMemcpy(tmp05,tmp04,SIZE,cudaMemcpyHostToDevice);
