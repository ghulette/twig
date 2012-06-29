float *tmp01,*tmp02,*tmp03,*tmp04,*tmp05,*tmp06,*tmp07;

cudaMalloc((void **)&tmp02,SIZE);
cudaMemcpy(tmp02,tmp01,SIZE,cudaMemcpyHostToDevice);

foo <<<N_BLOCKS,BLOCK_SIZE>>> (tmp02, N);
tmp03 = tmp02;

tmp04 = malloc(SIZE * sizeof(float));
cudaMemcpy(tmp04,tmp03,SIZE,cudaMemcpyHostToDevice);

cudaMalloc((void **)tmp05,SIZE);
cudaMemcpy(tmp05,tmp04,SIZE,cudaMemcpyHostToDevice);

bar <<<N_BLOCKS,BLOCK_SIZE>>> (tmp05,N);
tmp06 = tmp05;

tmp07 = malloc(SIZE * sizeof(float));
cudaMemcpy(tmp07,tmp06,SIZE,cudaMemcpyHostToDevice);
