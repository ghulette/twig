class Hello {
  private native void print();
  
  public static void main (String[] args) {
    new Hello().print();
  }
  
  static {
    System.loadLibrary("Hello");
  }
}
