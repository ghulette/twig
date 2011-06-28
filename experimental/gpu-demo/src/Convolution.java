import java.io.File;
import java.io.IOException;
import java.util.Arrays;

public class Convolution {
  public native void test();
  
  public double[] apply(double[] data) {
    return Arrays.copyOf(data,data.length);
  }
  
  static {
    // This is not ideal, assumes a very particular directory structure.
    try {
      File dir = new File(".");
      String pwd = dir.getCanonicalPath();
      System.load(pwd + "/build/lib/libconvolution.jnilib");
    }
    catch(IOException ex) {
      ex.printStackTrace();
    }
  }
}
