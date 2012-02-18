#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <OpenCL/opencl.h>

#define DATA_SIZE (1024)

const char *KernelSource = "\n" \
"__kernel void square(                   \n" \
"   __global float* input,               \n" \
"   __global float* output,              \n" \
"   const unsigned int count)            \n" \
"{                                       \n" \
"   int i = get_global_id(0);            \n" \
"   if(i < count) {                      \n" \
"       output[i] = input[i] * input[i]; \n" \
"   }                                    \n" \
"}                                       \n" \
"\n";

////////////////////////////////////////////////////////////////////////////////

cl_device_id getDeviceId() {
  int err;
  cl_device_id did;
  // Use CL_DEVICE_TYPE_GPU for GPU
  err = clGetDeviceIDs(NULL, CL_DEVICE_TYPE_CPU, 1, &did, NULL);
  if(err != CL_SUCCESS) {
    printf("Error: Failed to create a device group!\n");
    exit(1);
  }
  return did;
}

cl_context createContext(cl_device_id did) {
  int err;
  cl_context context;
  context = clCreateContext(0, 1, &did, NULL, NULL, &err);
  if(!context) {
    printf("Error: Failed to create a compute context!\n");
    exit(1);
  }
  return context;
}

cl_command_queue createCommandQueue(cl_device_id did, cl_context c) {
  int err;
  cl_command_queue q;
  q = clCreateCommandQueue(c, did, 0, &err);
  if(!q) {
    printf("Error: Failed to create a command commands!\n");
    exit(1);
  }
  return q;
}

cl_kernel compileProgram(const char **src, cl_device_id did, cl_context c) {
  int err;
  cl_program program;
  cl_kernel kernel;
  program = clCreateProgramWithSource(c, 1, src, NULL, &err);
  if(!program) {
    printf("Error: Failed to create compute program!\n");
    exit(1);
  }
  err = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
  if(err != CL_SUCCESS) {
    size_t len;
    char buffer[2048];
    printf("Error: Failed to build program executable!\n");
    clGetProgramBuildInfo(program, did, CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, &len);
    printf("%s\n", buffer);
    exit(1);
  }
  kernel = clCreateKernel(program, "square", &err);
  if(!kernel || err != CL_SUCCESS) {
    printf("Error: Failed to create compute kernel!\n");
    exit(1);
  }
  clReleaseProgram(program); // Maybe not here?
  return kernel;
}

cl_mem createInputFloatBuffer(cl_context c, int size) {
  cl_mem buff;
  buff = clCreateBuffer(c, CL_MEM_READ_ONLY, sizeof(float) * size, NULL, NULL);
  if(!buff) {
    printf("Error: Failed to allocate device memory!\n");
    exit(1);
  }
  return buff;
}

cl_mem createOutputFloatBuffer(cl_context c, int size) {
  cl_mem buff;
  buff = clCreateBuffer(c, CL_MEM_WRITE_ONLY, sizeof(float) * size, NULL, NULL);
  if(!buff) {
    printf("Error: Failed to allocate device memory!\n");
    exit(1);
  }
  return buff;
}

void copyFloatsToDevice(cl_command_queue q, cl_mem buff, float *data, int size) {
  int err;
  err = clEnqueueWriteBuffer(q, buff, CL_TRUE, 0, sizeof(float) * size, data, 0, NULL, NULL);
  if(err != CL_SUCCESS) {
    printf("Error: Failed to write to source array!\n");
    exit(1);
  }
}

void copyFloatsFromDevice(cl_command_queue q, cl_mem buff, float *data, int size) {
  int err;
  err = clEnqueueReadBuffer(q, buff, CL_TRUE, 0, sizeof(float) * size, data, 0, NULL, NULL );  
  if(err != CL_SUCCESS) {
    printf("Error: Failed to read output array! %d\n", err);
    exit(1);
  }
}

void runKernel(cl_device_id did, cl_kernel k, cl_command_queue q, cl_mem input, cl_mem output, int size) {
  size_t local;
  size_t global = size;
  int err  = 0;
  err |= clSetKernelArg(k, 0, sizeof(cl_mem), &input);
  err |= clSetKernelArg(k, 1, sizeof(cl_mem), &output);
  err |= clSetKernelArg(k, 2, sizeof(unsigned int), &size);
  if(err != CL_SUCCESS) {
    printf("Error: Failed to set kernel arguments! %d\n", err);
    exit(1);
  }
  err = clGetKernelWorkGroupInfo(k, did, CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, NULL);
  if(err != CL_SUCCESS) {
    printf("Error: Failed to retrieve kernel work group info! %d\n", err);
    exit(1);
  }
  err = clEnqueueNDRangeKernel(q, k, 1, NULL, &global, &local, 0, NULL, NULL);
  if(err) {
    printf("Error: Failed to execute kernel!\n");
    exit(1);
  }
}
  

int main(int argc, char** argv) {
  int err;                            // error code returned from api calls
      
  float data[DATA_SIZE];              // original data set given to device
  float results[DATA_SIZE];           // results returned from device
  unsigned int correct;               // number of correct results returned
  
  cl_device_id device_id;             // compute device id 
  cl_context context;                 // compute context
  cl_command_queue commands;          // compute command queue
  cl_kernel kernel;                   // compute kernel
    
  cl_mem input;                       // device memory used for the input array
  cl_mem output;                      // device memory used for the output array
    
  // Fill our data set with random float values
  int i = 0;
  unsigned int count = DATA_SIZE;
  for(i = 0; i < count; i++) {
    data[i] = rand() / (float)RAND_MAX;
  }
    
  device_id = getDeviceId();
  context = createContext(device_id);
  commands = createCommandQueue(device_id, context);
  kernel = compileProgram(&KernelSource, device_id, context);
  input = createInputFloatBuffer(context, DATA_SIZE);
  output = createOutputFloatBuffer(context, DATA_SIZE);
  copyFloatsToDevice(commands, input, data, DATA_SIZE);
  runKernel(device_id, kernel, commands, input, output, DATA_SIZE);
  clFinish(commands);
  copyFloatsFromDevice(commands, output, results, count);
    
  // Validate our results
  correct = 0;
  for(i = 0; i < count; i++) {
    if(results[i] == data[i] * data[i]) {
      correct++;
    }
  }
    
  // Print a brief summary detailing the results
  printf("Computed '%d/%d' correct values!\n", correct, count);
    
  // Shutdown and cleanup
  clReleaseMemObject(input);
  clReleaseMemObject(output);
  clReleaseKernel(kernel);
  clReleaseCommandQueue(commands);
  clReleaseContext(context);
  return 0;
}
