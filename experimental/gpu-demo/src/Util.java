import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;

public class Util {
  static double[] getGrayPixels(BufferedImage img) {
    int width = img.getWidth();
    int height = img.getHeight();
    double[] raw = null;
    raw = img.getRaster().getPixels(0,0,width,height,raw);
    double[] data = new double[width*height];
    for(int y=0; y < height; y++) {
      for(int x=0; x < width; x++) {
        data[y*width+x] = raw[(y*width+x)*3  ] / 3.0 +
                          raw[(y*width+x)*3+1] / 3.0 +
                          raw[(y*width+x)*3+2] / 3.0;
        data[y*width+x] = data[y*width+x] / 255.0;
      }
    }
    return data;
  }
  
  static BufferedImage imageFromArray(int w, int h, double[] data) {
    int type = BufferedImage.TYPE_BYTE_GRAY;
    BufferedImage img = new BufferedImage(w,h,type);
    WritableRaster r = img.getRaster();
    int[] scaled_data = new int[data.length];
    for(int i=0; i < data.length; i++) {
      scaled_data[i] = (int)(data[i] * 255.0);
    }
    r.setPixels(0,0,w,h,scaled_data);
    return img;
  }
}
