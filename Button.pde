class Button {
 PImage image, hover;
 float x,y;
 float width, height;
 String name;
 
 Button(String name, float x, float y){ // Buttons should receive a name and a position
   this.name = name;
   this.image=loadImage("buttons/" + name + ".png"); 
   this.hover=loadImage("buttons/" + name + "_hover" + ".png");
   this.x = x;
   this.y = y;
 }
 
 String getName() {
   return this.name;
 }
 

 boolean isClicked() {
   return (mouseX < x + 350  && mouseX > x && mouseY < y + 100 && mouseY > y);
 }
 
 void drawBtn() {
   if (isClicked()) {
     fill(70);
     noStroke();
     ellipse(x, y+50,50,50);
     ellipse(x+400, y+50, 50,50);
     image(this.hover, x, y);
   }
   else {
     fill(15);
     noStroke();
     ellipse(x, y+50,50,50);
     ellipse(x+400, y+50, 50,50);
     image(this.image, x, y);
   }
 }
}
