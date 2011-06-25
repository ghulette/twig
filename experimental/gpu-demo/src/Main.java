import java.io.File;
import java.io.IOException;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;

public class Main {
  public static void main(String[] args) {
    try {
      BufferedImage img = ImageIO.read(new File(args[0]));
      System.out.println("size=" + img.getWidth() + "x" + img.getHeight());
    }
    catch(IOException ex) {
      ex.printStackTrace();
    }
  }
}
