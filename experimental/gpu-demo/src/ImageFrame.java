import java.awt.*;
import java.awt.image.BufferedImage;

public class ImageFrame {
  private Frame frame;
  
  class ImageCanvas extends Canvas {
    private BufferedImage img;
    
    ImageCanvas(BufferedImage img) {
      this.img = img;
    }
    
    public Dimension getPreferredSize() {
      return new Dimension(img.getWidth(),img.getHeight());
    }
    
    public void paint(Graphics g) {
      g.drawImage(img,0,0,null);
    }
  }
  
  public ImageFrame(String title,BufferedImage img) {
    this.frame = new Frame(title);
    frame.add(new ImageCanvas(img));
    frame.pack();
    frame.setVisible(true);
  }
  
  public void setLocation(int x,int y) {
    this.frame.setLocation(x,y);
  }
}
