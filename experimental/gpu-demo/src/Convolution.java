import java.io.File;
import java.io.IOException;
import java.util.Arrays;

public class Convolution {
  public native double[] apply(double[] data);
  
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
