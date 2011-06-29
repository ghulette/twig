import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;

public class Main {  
  public static void main(String[] args) {
    try {
      BufferedImage img = ImageIO.read(new File(args[0]));
      if(img.getType() != BufferedImage.TYPE_3BYTE_BGR) {
        System.out.println("Wrong image type");
        System.exit(0);
      }
      int w = img.getWidth();
      int h = img.getHeight();
      double[] srcData = Util.getGrayPixels(img);
      BufferedImage srcImg = Util.imageFromArray(w,h,srcData);
      ImageFrame f1 = new ImageFrame("Before",srcImg);
      Convolution c = new Convolution();
      double[] postData = c.applyOpt(srcData);
      BufferedImage postImg = Util.imageFromArray(w,h,postData);
      ImageFrame f2 = new ImageFrame("After",postImg);
      f2.setLocation(w+10,0);
    }
    catch(IOException ex) {
      ex.printStackTrace();
    }
  }
}
